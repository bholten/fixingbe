# Fixing Bio-Engineer

Star Wars: Galaxies was a fantastic MMORPG that was unfortunately shut down 
2011. [Various emulators](https://swgemu.com) exist to recapture the magic, 
and one of the most compelling and complex systems in the game was 
Bio-Engineering.

This is a repository of data and analysis used as an attempt to
reverse-engineer how Bio-Engineering worked.

## Data

The data is scraped from [furrycat's
archive](http://pets.furrycat.net/) and reproduced here. The scripts
for scraping can be found under the `scripts/` folder, but they should
not be needed, as the sources are committed to this repo.

The data scraping currently makes several CSV files.

## Analysis

The analysis of the data sets are on-going. The `Rmd` files are found
here and the current state is published at [the GitHub
Pages](bholten.github.io/fixingbe/).

Most of the current analysis starts with
[GAMs](https://en.wikipedia.org/wiki/Generalized_additive_model) and then
attempts to narrow down the exact mechanics from relative influence
that the additive model suggests.
