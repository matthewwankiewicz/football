import os
import re
import numpy as np
import pandas as pd
from sklearn.cluster import KMeans

# ----------------------------
# Config
# ----------------------------
PROJ_CSV = "fantasy/draft25/projections_r.csv"   # your tibble export
ADP_CSV  = "fantasy/draft25/adp.csv"             # has one or more ADP columns (e.g., adp_nfc, adp_sleeper)
BOOM_CSV = "fantasy/draft25/boom_sheet.csv"      # optional; used only for a few context fields if present
OUT_DIR  = "fantasy/draft25/"            # output dir for final rankings + debug
os.makedirs(OUT_DIR, exist_ok=True)

# Composite weights (no injuries, no boom_rate)
w = {
    "strength": 0.60,
    "boom":     0.20,   # from VOR spread (ceiling_vor - points_vor)
    "adp":      0.15,
    "floor":    0.05,
}

# Pos caps for 1-QB leagues (loosen/remove for Superflex)
POS_CAP = {"QB": 2.5, "RB": 3.5, "WR": 3.5, "TE": 3.0}

# Tier counts (auto-clamped per-position by available rows)
TIERS_PER_POS = {"QB": 6, "RB": 8, "WR": 8, "TE": 6}


# ----------------------------
# Helpers
# ----------------------------
def ensure_player_column(df: pd.DataFrame, label="(unknown)"):
    """Try to ensure there's a 'player' column; otherwise raise with a helpful message."""
    if "player" in df.columns:
        return df
    for alt in ["player_name", "name", "Player", "PLAYER"]:
        if alt in df.columns:
            return df.rename(columns={alt: "player"})
    raise ValueError(f"{label} is missing a 'player' column. Add/rename it to 'player' and retry.")

def normalize_player_key(s: pd.Series) -> pd.Series:
    """Lowercase, trim, and strip common suffixes so merges work."""
    s = s.astype(str).str.replace(r"\s+(Jr\.|Sr\.|II|III)$", "", regex=True)
    return s.str.strip().str.lower()

def z_by_pos(df: pd.DataFrame, col: str) -> pd.Series:
    """Position-wise z-score; missing values preserved (we'll drop later)."""
    return df.groupby("pos")[col].transform(lambda x: (x - x.mean()) / (x.std(ddof=0) + 1e-9))

def mean_of_numeric_cols(rowframe: pd.DataFrame) -> pd.Series:
    """Row-wise mean of numeric columns (coerce non-numeric to NaN first)."""
    num = rowframe.apply(pd.to_numeric, errors="coerce")
    return num.mean(axis=1)

def pick_merge_keys(left: pd.DataFrame, right: pd.DataFrame):
    """Use (player_key, pos) if both have pos; else just player_key."""
    keys = ["player_key"]
    if "pos" in left.columns and "pos" in right.columns:
        keys.append("pos")
    return keys


# ----------------------------
# Load & Prepare
# ----------------------------
proj = pd.read_csv(PROJ_CSV)
adp  = pd.read_csv(ADP_CSV)

proj = ensure_player_column(proj, "projections_r.csv")
adp  = ensure_player_column(adp, "adp.csv")

proj["player_key"] = normalize_player_key(proj["player"])
adp["player_key"]  = normalize_player_key(adp["player"])

# Compute combined ADP from any columns starting with "adp"
adp_cols = [c for c in adp.columns if c.lower().startswith("adp")]
if not adp_cols:
    # fallback: if there's a single 'adp' column
    if "adp" in adp.columns:
        adp_cols = ["adp"]
    else:
        raise ValueError("No ADP columns found in adp.csv (expected columns like 'adp_nfc', 'adp_sleeper' or 'adp').")

adp["adp_combined"] = mean_of_numeric_cols(adp[adp_cols])

# Merge projections + ADP
merge_keys = pick_merge_keys(proj, adp)
df = proj.merge(adp[merge_keys + ["adp_combined"]], on=merge_keys, how="left", suffixes=("", "_adp"))

# Optional merge of boom/context sheet if present
if os.path.exists(BOOM_CSV):
    boom = pd.read_csv(BOOM_CSV)
    boom = ensure_player_column(boom, "boom_sheet.csv")
    boom["player_key"] = normalize_player_key(boom["player"])
    boom_cols_wanted = [
        # include whichever of these actually exist in your boom sheet:
        "o_line_total", "full_so_s_by_position", "qb_wr_te_boom",
        "team_depth_chart", "boom_factor",
        # sometimes alternative column names exist—include if present:
        "x1st_5_games_so_s_by_position", "x3rd_yr_wr", "full_so_s_by_position_1_14",
    ]
    present = ["player_key"] + [c for c in boom_cols_wanted if c in boom.columns]
    if len(present) > 1:
        df = df.merge(boom[present], on="player_key", how="left")

# ----------------------------
# Feature Engineering
# ----------------------------
# Strength: prefer VOR if present, else points
if "points_vor" in df.columns and df["points_vor"].notna().any():
    df["strength"] = df["points_vor"]
elif "points" in df.columns:
    df["strength"] = df["points"]
else:
    raise ValueError("Neither 'points_vor' nor 'points' found in projections.")

# Upside proxy: use VOR spread if available, else raw spread or sd_pts
if {"ceiling_vor", "points_vor"}.issubset(df.columns):
    df["upside_proxy"] = df["ceiling_vor"] - df["points_vor"]
elif {"ceiling", "points"}.issubset(df.columns):
    df["upside_proxy"] = df["ceiling"] - df["points"]
elif "sd_pts" in df.columns:
    df["upside_proxy"] = df["sd_pts"]
else:
    # if nothing available, set to 0 (will z-score to ~0 by position)
    df["upside_proxy"] = 0.0

# Floor: prefer floor_vor; else points - sd_pts; else 0
if "floor_vor" in df.columns:
    floor_base = "floor_vor"
elif {"points", "sd_pts"}.issubset(df.columns):
    df["floor_est"] = df["points"] - df["sd_pts"]
    floor_base = "floor_est"
else:
    df["floor_zero"] = 0.0
    floor_base = "floor_zero"

# z-scores within position
df["z_strength"]  = z_by_pos(df, "strength")
df["z_boom"]      = z_by_pos(df, "upside_proxy")           # this is your upside signal (no boom_rate)
df["z_adp_value"] = -z_by_pos(df, "adp_combined")          # earlier ADP (lower number) -> higher value
df["z_floor"]     = z_by_pos(df, floor_base)

# Cap by position (to prevent QB over-dominance in 1QB)
def cap_strength(row):
    cap = POS_CAP.get(row["pos"], None)
    val = row["z_strength"]
    return np.clip(val, -cap, cap) if cap is not None and pd.notna(val) else val
df["z_strength_capped"] = df.apply(cap_strength, axis=1)

# ----------------------------
# Score
# ----------------------------
required_for_score = ["z_strength_capped", "z_boom", "z_adp_value", "z_floor"]

df["Score"] = (
    w["strength"] * df["z_strength_capped"] +
    w["boom"]     * df["z_boom"] +
    w["adp"]      * df["z_adp_value"] +
    w["floor"]    * df["z_floor"]
)

# ----------------------------
# Drop rows with missing inputs + write debug
# ----------------------------
need_cols = required_for_score + ["Score"]
missing_mask = df[need_cols].isna().any(axis=1)

def which_missing(row):
    return ",".join([c for c in need_cols if pd.isna(row[c])])

debug = df.loc[missing_mask].copy()
if not debug.empty:
    debug["missing_cols"] = debug.apply(which_missing, axis=1)
    debug_cols = ["player", "team", "pos"] + need_cols + ["missing_cols"]
    debug.to_csv(os.path.join(OUT_DIR, "debug_missing_score_inputs.csv"), index=False)
    print(f"Dropped {missing_mask.sum()} rows for missing inputs. "
          f"See {os.path.join(OUT_DIR, 'debug_missing_score_inputs.csv')}")
else:
    print("No rows dropped for missing inputs.")

df_ready = df.loc[~missing_mask].copy()

# ----------------------------
# Tiering (robust to small groups)
# ----------------------------
df_ready["Tier"] = np.nan
for pos, sub in df_ready.groupby("pos", dropna=False):
    if sub.empty:
        continue
    k_desired = TIERS_PER_POS.get(pos, max(4, int(np.sqrt(len(sub)))))
    k = max(1, min(k_desired, len(sub)))  # can't have more clusters than samples
    if k == 1:
        df_ready.loc[sub.index, "Tier"] = 1
        continue

    km = KMeans(n_clusters=k, n_init=10, random_state=42)
    labels = km.fit_predict(sub[["Score"]].to_numpy())

    centers = km.cluster_centers_.ravel()
    order   = pd.Series(centers).rank(ascending=False, method="dense").astype(int).to_numpy()
    tier_map = {i: order[i] for i in range(k)}
    df_ready.loc[sub.index, "Tier"] = [tier_map[l] for l in labels]

# Merge Tier back so dropped players appear with Tier=NaN in the final board
df["Tier"] = df.get("Tier", np.nan)
df.loc[df_ready.index, "Tier"] = df_ready.loc[df_ready.index, "Tier"]

# ----------------------------
# Save final board
# ----------------------------
df = df.sort_values(["pos", "Tier", "Score", "adp_combined"], ascending=[True, True, False, True])

out_path = os.path.join(OUT_DIR, "output_rankings.csv")
df.to_csv(out_path, index=False)
print(f"Wrote {out_path}")
