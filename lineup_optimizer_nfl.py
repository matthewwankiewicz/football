#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Jan  3 18:50:29 2024

@author: matthew
"""

import pandas as pd
import pulp

# Positions
positions = ['QB', 'RB', 'WR', 'TE', 'DST']

# Load the data
dk_lines = pd.read_csv("dk_lines.csv")

# Remove rows with NaN in relevant columns
dk_lines = dk_lines.dropna(subset=['AvgPointsPerGame', 'avg_pts_allowed'])


# Assign weights to the variables
weight_dfs_points = 1  # Higher weight for dfs_points
weight_avg_pts = 1  # Lower weight for avg_pts

# Calculate the weighted composite score
dk_lines['composite_score'] = (
    (dk_lines['avg_pts_allowed'] * weight_dfs_points) + 
    (dk_lines['AvgPointsPerGame'] * weight_avg_pts)
) / (weight_dfs_points + weight_avg_pts)

dk_lines[dk_lines['composite_score'] > 0]

## remove teams that are not in slate

dk_lines = dk_lines[dk_lines['Roster Position'] != "CPT"]
dk_lines = dk_lines[dk_lines['TeamAbbrev'] != "MIA"]


dk_lines = dk_lines[dk_lines["Name"] != "Anthony Richardson"]
dk_lines = dk_lines[dk_lines["Name"] != "Lamar Jackson"]
dk_lines = dk_lines[dk_lines["Name"] != "Zay Flowers"]
dk_lines = dk_lines[dk_lines["Name"] != "J.K. Dobbins"]
dk_lines = dk_lines[dk_lines["Name"] != "Mark Andrews"]
dk_lines = dk_lines[dk_lines["Name"] != "Mitch Trubisky"]
dk_lines = dk_lines[dk_lines["Name"] != "T.J. Hockenson"]
dk_lines = dk_lines[dk_lines["Name"] != "Salvon Ahmed"]
dk_lines = dk_lines[dk_lines["Name"] != "Joshua Dobbs"]
dk_lines = dk_lines[dk_lines["Name"] != "Kirk Cousins"]
dk_lines = dk_lines[dk_lines["Name"] != "Tank Dell"]
dk_lines = dk_lines[dk_lines["Name"] != "Jeff Driskel"]
dk_lines = dk_lines[dk_lines["Name"] != "Skyy Moore"]



def optimize_lineup(dk_lines_data, budget, n_players, team_val, pos_restriction, captain=False, weight_avg_pts=0.5, weight_comp_score=0.5):
    # Create a pulp linear programming model
    model = pulp.LpProblem("Optimize NFL Lineup", pulp.LpMaximize)

    # Create a decision variable for each player
    player_vars = pulp.LpVariable.dicts("Players", dk_lines_data['Name'], 0, 1, cat='Integer')

    # Create captain decision variable if captain is True
    captain_vars = {}
    if captain:
        captain_vars = pulp.LpVariable.dicts("Captain", dk_lines_data['Name'], 0, 1, cat='Integer')

    # Objective function: Maximize weighted sum of total avg_pts and total composite_score
    total_avg_pts = pulp.lpSum(player_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'AvgPointsPerGame'].values[0] 
                               for player in dk_lines_data['Name'])
    total_comp_score = pulp.lpSum(player_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'composite_score'].values[0] 
                                  for player in dk_lines_data['Name'])

    if captain:
        total_avg_pts += 0.5 * pulp.lpSum(captain_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'AvgPointsPerGame'].values[0] 
                                          for player in dk_lines_data['Name'])
        total_comp_score += 0.5 * pulp.lpSum(captain_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'composite_score'].values[0] 
                                            for player in dk_lines_data['Name'])

    model += weight_avg_pts * total_avg_pts + weight_comp_score * total_comp_score

    # Constraint: Total salary must not exceed the budget
    total_salary = pulp.lpSum(player_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'Salary'].values[0] 
                              for player in dk_lines_data['Name'])
    if captain:
        total_salary += 0.5 * pulp.lpSum(captain_vars[player] * dk_lines_data.loc[dk_lines_data['Name'] == player, 'Salary'].values[0] 
                                         for player in dk_lines_data['Name'])
    model += total_salary <= budget

    # Constraint for captain
    if captain:
        model += pulp.lpSum(captain_vars[player] for player in dk_lines_data['Name']) == 1

    # Position constraints
    if pos_restriction:
        for pos in ['QB', 'DST']:  # Assuming these are the positions
            eligible_players = dk_lines_data[dk_lines_data['Position'].str.contains(pos)]['Name']
            model += pulp.lpSum(player_vars[player] for player in eligible_players) == 1  # At least one player from each position

        for pos in ['TE']:  # Assuming these are the positions
            eligible_players = dk_lines_data[dk_lines_data['Position'].str.contains(pos)]['Name']
            model += pulp.lpSum(player_vars[player] for player in eligible_players) >= 1  # At least one player from each position
            model += pulp.lpSum(player_vars[player] for player in eligible_players) <= 2 
            
        for pos in ['RB']:  # Assuming these are the positions
            eligible_players = dk_lines_data[dk_lines_data['Position'].str.contains(pos)]['Name']
            model += pulp.lpSum(player_vars[player] for player in eligible_players) >= 2  # At least one player from each position
            model += pulp.lpSum(player_vars[player] for player in eligible_players) <= 3 
            
        for pos in ['WR']:  # Assuming these are the positions
            eligible_players = dk_lines_data[dk_lines_data['Position'].str.contains(pos)]['Name']
            model += pulp.lpSum(player_vars[player] for player in eligible_players) >= 3  # At least one player from each position
            model += pulp.lpSum(player_vars[player] for player in eligible_players) <= 4 
            
    # Team constraint: No more than team_val players from a single team
    for team in dk_lines_data['TeamAbbrev'].unique():
        team_players = dk_lines_data[dk_lines_data['TeamAbbrev'] == team]['Name']
        model += pulp.lpSum(player_vars[player] for player in team_players) <= team_val

    # Constraint: Exactly n_players must be selected
    model += pulp.lpSum(player_vars[player] for player in dk_lines_data['Name']) == n_players

    # Solve the model
    status = model.solve()
    if status != pulp.LpStatusOptimal:
        return "No optimal solution found"

    # Retrieve the optimal lineup and captain if applicable
    optimal_lineup = [player for player in dk_lines_data['Name'] if player_vars[player].value() == 1]
    optimal_players = dk_lines_data[dk_lines_data['Name'].isin(optimal_lineup)]

    if captain:
        optimal_captain = [player for player in dk_lines_data['Name'] if captain_vars[player].value() == 1]
        optimal_players['IsCaptain'] = optimal_players['Name'].isin(optimal_captain)
        optimal_players_selected = optimal_players[['Name', 'TeamAbbrev', 'Position', 'Salary', 'IsCaptain', 'AvgPointsPerGame']]
    else:
        optimal_players_selected = optimal_players[['Name', 'TeamAbbrev', 'Position', 'Salary', 'AvgPointsPerGame']]

    return optimal_players_selected


# Example usage
optimal_lineup = optimize_lineup(dk_lines, 50000, n_players = 9, team_val = 3, pos_restriction = True, captain = False)
optimal_lineup