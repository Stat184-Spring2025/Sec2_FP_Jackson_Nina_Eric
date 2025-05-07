# Historical Analysis of Olympic Data

This repository is exploring different aspects of Olympic competition data. The data files include information on individual competitors in Olympics dating from 1896-2016, giving over 120 years of data.

## Overview

The main data file, athlete_events.csv contains information on individual athletes who competed in Olympic Games. A case is a single athlete, in a single event, in a single year. This means that there is data on athletes who have competed in several Olympic Games and several events. Each case has 15 variables: ID, Name, Sex, Age, Height, Weight, Team, NOC, Games, Year, Season, City, Sport, Event, and Medal. 

We decided to focus more on quantifiable data, such as Age, Height, and Weight, for further analysis. 

### Interesting Insight

![](medalsPerCountry.png,{fig-align="center",width="300"})

The image shown above is how many medals the top ten medal getters of Olympic history have. This displays how much the United States has dominated over the last 120 years. It is also important to note that, as of today, the Soviet Union is still in second place while the United States has participated in the four Olympics not included in this data set. This means that the gap between United States and second place is continuing to widen which each Olympic games.

## Repo Structure

- **FinalProj.qmd**: This was the template of the qmd file that we all added to in our own branches.
- **FinalProjMERGED.qmd**: This is the final qmd file of all of our work.
- **MLA9.csl** & **apa7.csl**: Two csl formats provided in the template (we chose apa7.csl).
- **references.bib**: Bib file for our references.
- **Project_Guidelines.md**: The rubric/outline we used to make our qmd file.
- **README.md**: Description of the repository.
- **.gitignore**: Ignored changes.
- **medalsPerCountry.png**: Image of one of our data visualizations.

## Data Sources and Acknowledgements

Main data is contained here: https://huggingface.co/datasets/EFarrallpsu/STAT184_Eric_Jackson_Nina/tree/main
taken from: https://www.kaggle.com/datasets/heesoo37/120-years-of-olympic-history-athletes-and-results

NBA Players: https://www.kaggle.com/datasets/drgilermo/nba-players-stats?resource=download

More information and other references found at the bottom References section in our pdf render.

## Authors

Nina Mesyngier - Applied Data Science student at Penn State University - Contact: nvm5600@psu.edu - LinkedIN: https://www.linkedin.com/in/ninamesyngier  
Jackson Gasperack - Computational Data Science Undergrad at Penn State University -
Contact: jpg6383@psu.edu - LinkedIn: https://www.linkedin.com/in/jackson-gasperack/   
Give information about who are the authors of the project and how people can get in touch if they have questions.
