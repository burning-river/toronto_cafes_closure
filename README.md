# Toronto Cafe Closures

## Introduction

One of the prime considerations for any business owner looking to launch a new store is location. A great location includes an ample local consumer-base, easy accessibility, affordable rent or cost of space and low competition from other similar establishments. We analyze spatial variables to study their impact on the success of coffee shops or cafes in the city of Toronto. By studying the factors that influence success or failures, we can help avoid financial losses, suggest optimal resource allocation and enable informed decision-making for prospective business owners. More specifically, the goal of this work was to predict store closures and develop a strategy for identifying optimal locations for opening new cafes. We downloaded and scraped relevant spatial data such as a list of open cafes from 2016 [Link](https://www.kaggle.com/datasets/kevinbi/toronto-restaurants). The ground truth or label indicating whether the store is open or closed today was obtained from scraping Google search results for each store. We then engineered features such as whether the cafe was a chain or not, proximity of the cafes to Toronto downtown and subway stations, availability of other cafes and restaurants nearby (both chain and non-chain cafes), availability of parking lots nearby and estimated rent of commercial space around the stores. We trained a spatial model to predict store closures from the features. Lastly, we scraped locations of available commercial properties up for sale in the city and recommended a few spots that our model predicts to be optimum for opening a cafe. 
