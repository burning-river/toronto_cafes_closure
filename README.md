# Toronto Cafe Closures

## Introduction

One of the prime considerations for any business owner looking to launch a new store is location. A great location includes an ample local consumer-base, easy accessibility, affordable rent or cost of space and low competition from other similar establishments. In this work, we analyze spatial factors to understand their impact on the success of coffee shops or cafes in the city of Toronto. By studying the factors that influence success or failures, we can help avoid financial losses, suggest optimal resource allocation and enable informed decision-making for prospective business owners. More specifically, the goal of this work is to predict store closures and develop a strategy for identifying optimal locations for opening new cafes. We downloaded and scraped relevant spatial data such as a list of open cafes from 2020 [Kaggle](https://www.kaggle.com/datasets/kevinbi/toronto-restaurants). The ground truth or label indicating whether the store is open or closed today was obtained from scraping Google search results for each store. We then engineered features such as whether the cafe was a chain or not, proximity of the cafes to Toronto downtown and subway stations, availability of other cafes and restaurants nearby (both chain and non-chain cafes), availability of parking lots nearby and estimated rent of commercial space around the stores. We trained a spatial model to predict store closures from the features. Lastly, we scraped locations of available commercial properties up for sale in the city and recommended a few spots that our model predicts to be optimum for opening a cafe. 

## Data Sources

* __Toronto Restaurants:__ This dataset contains information about 12,227 open restaurants
in Toronto including the restaurant name and 125 cuisine types (Afghan, Café, Mexican etc.). The dataset is from the year 2020 and we filtered our dataset to include cafes (ncafes = 453) only. The cafe subset included 374 non-chain/independent cafes
and 79 chain cafes such as Aroma Espresso Bar, Delimark Cafe, Starbucks and Tim
Horton's. Spatially, the cafes were more concentrated in the Old Toronto area which
includes downtown (165 cafes in 120 km2 area) and more spread out in the rest of the
city (258 cafes in 544 km2 area). Additionally, since the Kaggle dataset was 4 years old, we gathered the labels by
programatically looking up the name and address of each restaurant on Google and
searching for the phrase ‘permanently closed’ in the html text. The location of open
and closed cafes throughout the city and a closer look at downtown
region are shown in figures below. In total, the cafes in the city experienced a 27% closure rate
in the past 4 years.
<p float="left">
<img src="figures/cafe_locations.png" width="500" height="300"/>
<img src="figures/downtown_cafes.png" width="500" height="300"/>
</p>

* __Toronto Commercial Listings for Lease:__ The Toronto commercial listings data
was obtained from the Zolo website [Zolo](https://www.zolo.ca/toronto-real-estate/commercial-for-lease). We scraped details including rental address and price per square feet for 644 listings in the city. We converted the address information
to longitude and latitude information using the Geodata API. The rents ranged from
$1 to $900 per square feet with an average of $34 in downtown and $21 in the rest of
Toronto. We will later predict the viability of these locations for opening new cafes. The figure below shows the available commercial listings in the city.
<p>
<img src="figures/rental_space.png" width="400" height="300"/>
</p>

* __Toronto Population Data:__ The dataset contains information about the population
of Toronto divided into small blocks along with their spatial information [Open Canada](https://open.canada.ca/data/en/dataset/32f1a777-9fcf-4e4a-8c66-82c66a2e76f1). This provides us with an estimate of the number of customers available locally around each cafe.

* __Open Street Map (OSM) Data:__ We also pulled data from the OpenStreetMap. The
relevant data is mentioned below.

  – __Toronto subway, streetcar and city bus stations:__ Contains information
about the subway, street car and bus stops in the city. In total there were 7118
such stops most of which were located along the streets of Toronto. For illustration, we can see the arrangement of public transport stops in downtown below.
<p>
<img src="figures/public_transport_downtown.png" width="400" height="300"/>
</p>

  – __Downtown Boundary:__ This boundary defines Old Toronto and allows for comparisons
between downtown and other regions.

  – __Parking Lots:__ Contains longitude and latitude of 5400 free parking spots in
Toronto. Free parking space should be associated with greater accessibility increasing
the nearby cafe visits.

  – __Street Network:__ Contains 11045 primary and secondary roads and highways in
Toronto.

  – __Local Attractions:__ Contains locations and shapes of all tourist spots in the city
including zoos, museums, parks, etc. In total there were 94 such spots.

## Methods

### Data pre-processing

Even though the raw dataset contained cafe information in Toronto, there were 150 observations
that were outside the city border (perhaps within the Greater Toronto Area or GTA)
and therefore we dropped them in our analysis. The label for cafes indicating whether they
are closed or not was validated by manually searching the names and addresses of some
randomly closed cafes on Google. Additionally, the cafe name information was in the form
of unstructured text. For example, while identifying chain cafes, the ‘Tim Horton's Cafe’
was incorrectly spelled as ‘Tim Horton’, ‘Tim Horton Donuts’ or ‘Tim Hortons’ in a few
cases. We included every misspelled version of the cafe along with the correct name in the
list of chain cafes. We then used string comparison of each cafe in the dataset to the list of
chains to determine if it was a chain or not.

The commercial property rental dataset contained 380 duplicate values with identical location
and rent and therefore we removed them. Finally, the Toronto population data contained 21,972 values from the GTA and 2 null values
that we dropped from downstream analysis.

### Feature engineering 

Through our wide collection of datasets, we created the following features:
• Chain cafe (binary): Whether the cafe name is close to ‘Aroma Espresso Bar’, ‘Delimark
Cafe’, ‘Starbucks’ or ‘Tim Horton’s’.

• Proximity to restaurants (continuous): We calculated the harmonic mean of distance of cafes to other restaurants (including all cuisines) in the city. Larger values indicate larger distances to restaurants in the city indicating less competition.

• Nearest distance to a chain cafe (continuous).

• Proximity to other cafes using the harmonic mean (continuous).

• Proximity to nearby parking lots (continuous) using the harmonic mean (continuous).

• Distance to nearest tourist attraction (continuous).

• Distance to nearest public transport stop (continuous).

• Proximity to stops (continuous).

• Distance to nearest street (continuous).

• Whether the cafe is located in downtown or not (binary).

• Number of customers nearby (continuous).

• Rental costs of cafes: We used the rent data from Zolo and a grid covering the
Toronto region to interpolate the rental cost across the city (continuous). We used the inverse
distance weighting technique for interpolation. The figure below shows
the interpolated rental distribution in the city. The highest rents were at the following places:

  - $900/sqft in 1420 Bayview Ave.
  - $800/sqft in B5-95 State Crown Blvd.
  - $5300/sqft MAIN-369 Eglinton Avenue W.

Lastly, we estimated the cafe rental costs by the rental cost
of the nearest grid point to each cafe.
<p>
<img src="figures/rent_idw.png" width="500" height="300"/>
</p> 

### Exploratory Data Analysis

#### Rent comparison between downtown and rest of Toronto
We observe that rents in downtown ($34) are appreciably higher than outer
regions ($20) with statistical significance ($p$ < 2.2e-16). However, the mean rent in areas around
the cafes doesn’t distinguish between open and closed cafes suggesting that rent does not
have significant association with cafe closure.
<p>
<img src="figures/rent_comparison.png" width="400" height="300"/>
</p>

#### Chain vs. independent cafe
We labelled each cafe in our data whether it is a chain cafe or not with the hypothesis that
chain cafes will be less susceptible to closures. This was validated in our data after performing
chi-square contingency test where we compare the observed vs. expected distribution of
cafe closures in chain vs. non-chain cafes. The discrepancy between the
distributions is statistically significant ($p$ −value = 1E-4). Chain cafes closed at only 9% rate compared to non-chain cafes at 31% whereas the overall closure rate was 27%.

Observed distribution of closure in chain
vs. non-chain cafes
|                   |  Closed  |  Open |
| :---------------- | :------: | ----: |
| Chain             |   7      | 72    |
| Not-chain         |   116    | 258   |

Expected distribution of closure in chain
vs. non-chain cafes
|                   |  Closed  |  Open |
| :---------------- | :------: | ----: |
| Chain             |   21     | 58    |
| Not-chain         |   102    | 272   |

#### Distance to main streets
We compared the nearest distance to a main street for all cafes with the hypothesis that main streets often have high vehicle and less pedestrian traffic. Therefore, closer proximity to main streets might be negatively correlated with closures. We see slight confirmation of that in the plot below where the open cafes were situated further away from main streets than closed cafes ($p$ = 0.03).
<p>
<img src="figures/nearest_main_street.png" width="450" height="300"/>
</p>

#### Local competitors
We compared the proximity of cafes to all restaurants in the city by calculating the harmonic mean of the distances. The hypothesis was that the further the distance between a cafe and restaurants in the city, lesser the competition and therefore lower the closure rate. We see some indication of that in the boxplot below where the median distance between open cafes and restaurants is larger than that for closed cafes ($p$ = 0.08). 
<p>
<img src="figures/num_restaurants.png" width="450" height="300"/>
</p>

## Predictive Modeling
Using our myriad data sources and engineered features, we trained a classification model that predicts if the cafe was closed or open. 

## Model building 
After extracting relevant location, cost of business and nearby competition
based features, we moved on to the predictive modeling stage of the work. We first began with studying the correlation between features. We plot below the correlation heatmap between the features and observed that no feature was highly correlated with other features in the dataset. 
<p>
<img src="figures/correlation_heatmap.png" width="800" height="400"/>
</p>

We trained a couple of classification models: elastic net and random forest classifier. We used
nested grid search spatial cross-validation to fine-tune our model with 5 outer folds and 5
repetitions. We chose the fine-tuned model based on the best AUC score.

Using the same features, we use the available commercial space rental data as the test set.
We predict closure probabilities for both independent/non-chain and chain cafes and plot the best and worst locations for both on the basemap of Toronto.

## Results
The AUC scores on the nested cross-validation sets were comparable for both the elastic net model (0.6 $\pm$ 0.09) and random forest (0.57 $\pm$ 0.08). However, the elastic net model performed no better on the test set than the baseline model with a closure probability of 0.27 (observed closure rate). We therefore selected the random forest classifier as our final model. The model does a modest job of predicting store closure. Below we plot the feature importances from the random forest model. Competition and accessibility based features such as proximity to the nearest chain cafe and other restaurants, and the number of nearby public transport stops were the top features identified by the model.
<p>
<img src="figures/feature_importances.png" width="500" height="300"/>
</p>

From the test set, we predict top and worst locations for non-chain and chain cafes based on closure probabilities in the
city as shown in the figures below. The red spots indicate the worst whereas the green ones the best.

### Independent cafes
Best and worst locations for opening independent cafes:
<p float="left">
<img src="figures/best_worst_locations_all_cafes.png" width="500" height="300"/>
<img src="figures/best_locations_zoomed.png" width="500" height="300"/>
</p>

The best locations were situated in the downtown/Old Toronto area due to better accessibility and location attractiveness. They had a mean closure probability of 0.17 and their addresses were as follows:

• 75, Victoria Street, King East, Toronto Centre, Old Toronto, Toronto, Ontario, M5C 2B1, Canada

• 9, Renfrew Place, Entertainment District, Spadina—Fort York, Old Toronto, Toronto, Ontario, M5V 1Z1, Canada

• 16, Dalhousie Street, Garden District, Toronto Centre, Old Toronto, Toronto, Ontario, M5C 1R9, Canada

• 195, Church Street, Garden District, Toronto Centre, Old Toronto, Toronto, Ontario, M5B 1Z2, Canada

• 30, Duncan Street, Entertainment District, Spadina—Fort York, Old Toronto, Toronto, Ontario, M5V 1W2, Canada

The worst locations with a mean closure probability of 0.69 are listed below and they were situated outside downtown.

• 200, Silver Star Boulevard, Scarborough—Agincourt, Scarborough, Toronto, Ontario, M1V 5H4, Canada

• 102, Galaxy Boulevard, Etobicoke North, Etobicoke, Toronto, Ontario, M9W 6J6, Canada

• Voyager Court North, Etobicoke North, Etobicoke, Toronto, Ontario, M9W 5P3, Canada

• 5, Lavington Drive, Martin Grove Gardens, Etobicoke Centre, Etobicoke, Toronto, Ontario, M9R 2J2, Canada

• Highway 401, Etobicoke North, Etobicoke, Toronto, Ontario, M9R 3T9, Canada

• Arrow Road, Humber River—Black Creek, North York, Toronto, Ontario, M9M 2L4, Canada

• 3650, Victoria Park Avenue, Don Valley North, Scarborough, North York, Toronto, Ontario, M1W 3S2, Canada

### Chain cafes
For chain cafes, the best and worst locations were as follows:

<p float="left">
<img src="figures/best_worst_locations_chain.png" width="500" height="300"/>
<img src="figures/best_locations_zoomed_chain.png" width="500" height="300"/>
</p>

The best locations were again situated in the downtown/Old Toronto area. They had a mean closure probability of 0.14 and the new addresses were as follows:

• Dundas Street West, Discovery District, Spadina—Fort York, Old Toronto, Toronto, Ontario, M5T 1G7, Canada

• 222, Spadina Avenue, Chinatown, Spadina—Fort York, Old Toronto, Toronto, Ontario, M5T 2C2, Canada

The worst locations with a mean closure probability of 0.69 are listed below and the new locations were situated outside downtown.

• 2010, Ellesmere Road, Scarborough—Guildwood, Scarborough, Toronto, Ontario, M1H 3B7, Canada

• 62, Scarsdale Road, Don Valley East, North York, Toronto, Ontario, M3B 2R2, Canada

• 1113, Finch Avenue West, York Centre, North York, Toronto, Ontario, M3J 2C5, Canada

• 18, Ashwarren Road, York Centre, North York, Toronto, Ontario, M3J 1N4, Canada
