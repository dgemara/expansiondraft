# expansiondraft

This is my undergraduate capstone project, to try to determine the optimal strategy for the Las Vegas Golden Knights, able to pluck a player from each team, with a number of constraints.

The project was originally done in Excel using Solver, but is now updated using R, specifically tidyverse and lpsolve.

The data comes from:

Corsica (RIP) - for GameScore

[Hockey-Reference](http://hockey-reference.com) - for point shares

[CapFriendly](http://capfriendly.com) - for cap hit, rookie vs protected data.

[Wikipedia](http://wikipedia.org) - for players that were actually protected.

The data was scraped (responsibly) from these sites, and the data files can be found in the 'data' folder.
