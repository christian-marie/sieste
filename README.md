sieste
======

REST interface to vaultaire

## Role
To provide abstractions of all things Vaultaire related to consumers over HTTP and JSON.

## Current coverage
### /simple/search
A simple interface to [Chevalier](https://github.com/anchor/chevalier), options avaliable are:
* `q`: the search query, blank means `*`.
* `address`: address of the source, blank is ignored
* `page`: the page requested for pagination.
* `page_size`: the number of results per page.
* `origin`: the origin you would like to search within

Parameters `q` and `address` should be used for searching and specific id listings, accordingly, and should not be used in conjuction with each other. 

### /interpolated/{origin}/{address}
Displays interpolated values retrieved from [readerd](https://github.com/anchor/vaultaire/blob/master/lib/Vaultaire/Reader.hs), options avaliable are:
* `address`: the unique identifier of a source (within an origin)
* `start`: unix epoch for start of search, no value before this will be returned.
* `end`: unix epoch for end of search, no value after this will be returned.
* `interval`: the distance between interpolated results, specified in seconds.
* `origin`: the origin you would like to search within, this is a temporary hack.
* `_float`: used to flag if the datapoints are floats, as opposed to integers (default), as dictacted by the return values in the `simple/source` result

The format for output is [time, value] where:
* `time`: is the unix timestamp for the interpolated value.
* `value`: is the linearly interpolated value between two numeric values

### /raw/{origin}/{address}
Renders points straight from [readerd](https://github.com/anchor/vaultaire/blob/master/lib/Vaultaire/Reader.hs) via [broker](https://github.com/anchor/vaultaire/blob/master/lib/Vaultaire/Broker.hs), no interpolation is done.
* `address`: the unique identifier of a source (within an origin)
* `start`: unix epoch  for start of search as an integer, no value before this will be returned.
* `end`: unix epoch for end of search as an integer, no value after this will be returned.
* `origin`: the origin you would like to search within
* `_float`: used to flag if the datapoints are floats, as opposed to integers (default), as dictacted by the return values in the `simple/source` result

The format for output is [time, value] where:
* time: is the unix timestamp, with extra precision in the form of a real number
* value: is the value represented
