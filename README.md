# Toronto Cafe Closures

## Introduction

One of the prime considerations for any business owner looking to launch a new store is location. A great location includes an ample local consumer-base, easy accessibility, affordable rent or cost of space and low competition from other similar establishments. We analyze spatial variables to study their impact on the success of coffee shops or cafes in the city of Toronto. By studying the factors that influence success or failures, we can help avoid financial losses, suggest optimal resource allocation and enable informed decision-making for prospective business owners. More specifically, the goal of this work was to predict store closures and develop a strategy for identifying optimal locations for opening new cafes. We downloaded and scraped relevant spatial data such as a list of open cafes from 2016 [Kaggle](https://www.kaggle.com/datasets/kevinbi/toronto-restaurants). The ground truth or label indicating whether the store is open or closed today was obtained from scraping Google search results for each store. We then engineered features such as whether the cafe was a chain or not, proximity of the cafes to Toronto downtown and subway stations, availability of other cafes and restaurants nearby (both chain and non-chain cafes), availability of parking lots nearby and estimated rent of commercial space around the stores. We trained a spatial model to predict store closures from the features. Lastly, we scraped locations of available commercial properties up for sale in the city and recommended a few spots that our model predicts to be optimum for opening a cafe. 

## Data Sources

* __Toronto Restaurants:__ This dataset contains information about 12,227 open restaurants
in Toronto including the restaurant name and 125 cuisine types (Afghan, Café, Mexican etc.). The dataset is from the year 2020 and we filtered our dataset to include cafes (ncafes = 453) only. The cafe subset included 374 non-chain/independent cafes
and 79 chain cafes such as Aroma Espresso Bar, Delimark Cafe, Starbucks and Tim
Horton's. Spatially, the cafes were more concentrated in the Old Toronto area which
includes downtown (165 cafes in 120 km2 area) and more spread out in the rest of the
city (258 cafes in 544 km2 area). Additionally, since the Kaggle dataset was 8 years old, we gathered the labels by
programatically looking up the name and address of each restaurant on Google and
searching for the phrase ‘permanently closed’ in the html text. The location of open
and closed cafes throughout the city and a closer look at downtown
region are shown in figure below. In total, the cafes in the city experienced a 27% closure rate
in the past 4 years.
<p float="left">
<img src="figures/cafe_locations.png" width="500" height="300"/>
<img src="figures/downtown_cafes.png" width="500" height="300"/>
</p>

* __Toronto Commercial Listings for Lease:__ The Toronto commercial listings data
was obtained from the Zolo website [Zolo](https://www.zolo.ca/toronto-real-estate/commercial-for-lease). We scraped details including rental address and price per square feet for 644 listings in the city. We converted the address information
to longitude and latitude information using the Geodata API. The rents ranged from
$1 to $900 per square feet with an average of $34 in downtown and $21 in the rest of
Toronto. We will later test the viability of these locations for opening new cafes. The figure below shows the available commercial listings in the city.
<p>
<img src="figures/rental_space.png" width="400" height="300"/>
</p>

* __Toronto Population Data:__ The dataset contains information about the population
of Toronto divided into small blocks along with their spatial information [Open Canada](https://open.canada.ca/data/en/dataset/32f1a777-9fcf-4e4a-8c66-82c66a2e76f1). Figure 3,
shows the population density (people count per km2) within the city. This provides us
with an estimate of the number of customers available locally around each cafe.
* __Open Street Map (OSM) Data:__ We also pulled data from the OpenStreetMap. The
relevant data is mentioned below.

  – __Toronto subway, streetcar and city bus stations:__ Contains information
about the subway, street car and bus stops in the city. In total there were 7118
such stops most of which were located along the streets of Toronto (Figure 4).

  – __Downtown Boundary:__ This boundary defines Old Toronto and allows for comparisons
between downtown and other regions.

  – __Parking Lots:__ Contains longitude and latitude of 5400 free parking spots in
Toronto. Free parking space should be associated with greater accessibility increasing
the nearby cafe visits.

  – __Street Network:__ Contains 11045 primary and secondary roads and highways in
Toronto.

  – __Local Attractions:__ Contains locations and shapes of all tourist spots in the city
including zoos, museums, parks, etc. In total there were 94 such spots.

