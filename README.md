sieste
======

REST interface to vaultaire

## Role
To provide abstractions of all things Vaultaire related to consumers over HTTP and JSON.


## Terminology

A **sourcedict** is a series of key-value pairs that identify a metric in the vault. These include a mandatory `address`, and zero or more metadata tags. 

The **address** of a metric is the unique identifer used to retrieve datapoints for the metric from the vault


## Current coverage

### /simple/search
Query available metrics via [Chevalier](https://github.com/anchor/chevalier).

* `q`: search query for any value of a key-value pair in a sourcedict
* `origin`: the origin you would like to search within
* `address`: the unique identifier of a source (within an origin)
* `page`: the page requested for pagination.
* `page_size`: the number of results per page.

Returns: a list of one or more sourcedicts, in the form of an array of strings, with comma-delimited key-value pairs, which are tilda-delimlited. e.g. `["key1~value1,key2~value2...",...]`

Sample API call: 

	    sieste.local/simple/search?origin=ABC123&q=*search* 

Sample result: 

	    ["address~XYZ456AA,keyone~value,keytwo~value", ...]

Parameters `q` and `address` should be used for searching and specific id listings, accordingly, and should not be used in conjuction with each other. 

### /interpolated/{origin}/{address}

Displays interpolated values retrieved from [readerd](https://github.com/anchor/vaultaire/blob/master/lib/Vaultaire/Reader.hs), options avaliable are:

* `origin`: the origin you would like to search within
* `address`: the unique identifier of a source (within an origin)
* `start`, `end`: unix epoch bounds for search
* `interval`: the distance between interpolated results, specified in seconds.
* `_float`: used to flag if the datapoints are floats, as opposed to integers (default), as dictacted by the return values in the `simple/source` result


Returns: an array of `time,value` pairs, where:

* `time`: is the unix timestamp for the interpolated value.
* `value`: is the linearly interpolated value between two numeric values. Integer, or float if `_float` is flagged.
	
Sample API call

	    sieste.local/interpolated/ABC123/XYZ456AA?start=1400000000&end=141000000&interval=10

Sample result: 

	    [[1400000000,10],[1400000010,20],[1400000030,30]...]

Sample result if using `_float`: 

	    [[1400000000,10.1],[1400000010,20.2],[1400000030,30.3]...]


### /raw/{origin}/{address}

Renders points straight from [readerd](https://github.com/anchor/vaultaire/blob/master/lib/Vaultaire/Reader.hs) via [broker](https://github.com/anchor/vaultaire/blob/master/lib/Vaultaire/Broker.hs), no interpolation is done.



* `origin`: the origin you would like to search within
* `address`: the unique identifier of a source (within an origin)
* `start`, `end`: unix epoch bounds for search
* `_float`: used to flag if the datapoints are floats, as opposed to integers (default), as dictacted by the return values in the `simple/source` result

The format for output is [time, value] where:

* time: is the unix timestamp, with extra precision in the form of a real number
* value: is the value represented. Integer, or float if `_float` is flagged.

Sample API call: 

		sieste.local/raw/ABC123/XYZ456AA?start=1400000000&end=141000000`

Sample result: 

		[[1400000000,10],[1400000100,100],[1400000200,200]...]
		
Sample result if using `_float`: 

		[[1400000000,10.1],[1400000100,100.1],[1400000200,200.2]...]
