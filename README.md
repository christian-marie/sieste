sieste
======

REST interface to vaultaire

## Role
To provide abstractions of all things Vaultaire related to consumers over HTTP and JSON.


## Current coverage
### /simple/search
A simple interface to [Chevalier](https://github.com/anchor/chevalier), options avaliable are:
* q: the search query, blank means *.
* page: the page requested for pagination.
* page_size: the number of results per page.

### /interpolated/{source}
Displays interpolated values retrieved from [readerd](https://github.com/anchor/vaultaire/blob/master/src/readerd.hs), options avaliable are:
* source: the tags of the source that you are requesting, specified as k~v,k2~v2.
* start: unix epoch for start of search, no value before this will be returned.
* end: unix epoch for end of search, no value after this will be returned.
* interval: the distance between interpolated results, specified in seconds.
* origin: the origin you would like to search within, this is a temporary hack.

The format for output is [time, value] where:
* time: is the unix timestamp for the interpolated value.
* value: is the linearly interpolated value between two numeric values, or a count
  of countable points within that period.

### /raw/{source}
Renders points straight from [readerd](https://github.com/anchor/vaultaire/blob/master/src/readerd.hs), no interpolation is done.
* source: the tags of the source that you are requesting, specified as k~v,k2~v2.
* start: unix epoch  for start of search as an integer, no value before this will be returned.
* end: unix epoch for end of search as an integer, no value after this will be returned.
* origin: the origin you would like to search within, this is a temporary hack.

The format for output is [type, time, value] where:
* type: is one of
  * counter
  * integer
  * real
  * text
  * blob
* time: is the unix timestamp, with extra precision in the form of a real number
* value: is the value represented in the cases of:
  * counter: as null
  * integer: as a number
  * real: as a number
  * text: as a string
  * blob: as a base64 encoded string
