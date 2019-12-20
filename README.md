# FIFA 20 ANALYSIS 
https://giuliamazza.shinyapps.io/FIFA20_analysis/

The FIFA20 analysis app is a tool to study the statistics of the soccer players included in FIFA 20, a football simulation video game published by Electronic Arts as part of the FIFA series. 
## The dataset
The starting dataset was composed of six tables, one per each year and 104 variables per each player. I decided to reduce the variables to 60, keeping the most interesting and valuable ones. Then I kept two tables, one with only the data from the year 2020, and one made by merging all the tables. Since the size of the dataset is very big, the app takes some minutes to charge completely. As I said at the end of this report, one of the improvements that I will try to do in the future is to add either a loading page that will cover the app for few seconds before showing the app or a loading bar that will disappear when the plots will be ready to be displayed.
## The app
The app consists of five parts:
•	Top 3 players,
•	Single-player analysis,
•	Players comparison,
•	Style analysis per club
•	Style analysis per country.
Each part analyses the dataset from a different perspective. The app opens on the first page (Top 3 players) and then can be navigated through the radio buttons in the sidebar. 
On the top right of the app, there are hyperlinks to my own LinkedIn profile and the Kaggle website, where the dataset was taken from.
### Top 3 players 
Once opened the app, the first page shown is the Top 3 Players. The page is divided horizontally into two. 
The first part shows the best players overall and their overall score out of 100. This part is static so the names of Messi, Ronaldo, and Neymar will always be shown.  
The second part shows the best three players according to the filters chosen in the sidebar. To make it easier for the user to understand what he is analysing I made the title of the second section reactive to the inputs of the filter. So, if he selects as role DEF and MID, and as nationality Argentina, Portugal and Italy, the title will be Best DEF, MID in Argentina, Portugal, Italy.
For each player, the app displays the relative club, position, overall score out of 100 and a radar chart with the value of the main abilities. The abilities of reference differ between players with goalkeeper role and the others. 
I found necessary to have the static part because it is important to have in mind who are the best players to make good comparisons.
### Single-player analysis
 The Single player page displays the main statistics for the player selected in the filter in the sidebar. Apart from the basic info, there are three parts.
•	Right under the basic info of the player, there is a tab panel that allows the user to visualize a radar chart per each category of ability: attacking, skill, movement, power, mentality, defending, goalkeeping.
•	The ability timeline uses shows the development of the abilities of the player through time. The table illustrates the percentual difference between the years 2015-2019 and 2018-2019. It is important to have both these values because it allows the user to understand better the journey of the player through time. Did the player improve from one year to another or was it a slow development? 
•	The last plot examines the value and the wage of the player compared to the other players. It is interesting to see how some players have a higher salary than others with a higher value.
### Players comparison
 The third page allows the user to compare the abilities of two players with radar charts. By clicking on the tab panels the graph changes according to the selected ability.
### Style analysis per club
 The Style analysis per club page has value boxes that visualize immediately the best club overall, in aggression and dribbling, filtered by the inputs in the sidebar. I chose these three values because I believe that these are the ones that change the most the style of a club. 
The graph below shows the situation of the clubs selected in each main ability. The values displayed are an average of all the players' rates. 
### Style analysis per country
 The last page has ten different maps according to the ability chosen to be displayed in the tab panel. Under the maps, a data table shows the values of each country.
## Future improvements
Of course, I realise that there are many things to improve and embellish and for this reason I made a list of further amelioration that I will work on:
•	Top 3 players page: 
o	I want to add a filter for the club so the user can see which player is the best in each team. 
o	I want to have first a filter for the continent and then the relative countries. To do it, I could use the package “countrycode” and create a new column in the dataset with the result. 
•	Single-player analysis and Players comparison page: 
o	I want to add a dataset that connects the name of the team to the image of the relative jersey and integrate it into the players’ basic info. I couldn’t do it because I couldn’t find one.
•	Style per club page: 
o	I want to replace the plot with a data table like the one in the Style per country page but filtered by the clubs selected in the filter.
•	Style per country page: 
o	As for the Top 3 players page, I want to have first a filter for the continent and then the relative countries,
o	I want to add a filter per country so the user can visualize only the countries of interest.
•	Improve the style
•	Creating loading page 

