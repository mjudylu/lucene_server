**Lucene Server** is an *Erlang* application that let's you manage and query documents using an **in-memory** [Lucene](http://lucene.apache.org) backend

## Usage
To start the application just run ``lucene_server:start().``

## Adding/Deleting documents
To add documents use: ``lucene:add(Docs).`` where ``Docs :: [lucene:doc()]``
Each document is a ``proplist`` where the keys can be atoms, strings or binaries and the values can be numbers, atoms, strings or ``lucene:geo()``.
To delete documents use: ``lucene:del(Query).`` where ``Query`` is written respecting the [Lucene Query Syntax](http://lucene.apache.org/core/3_6_0/queryparsersyntax.html).

## Querying
To find documents according to a query use: ``lucene:match(Query, PageSize).``, ``lucene:match(Query, PageSize, SortFields)`` or ``lucene:match(Query, PageSize, SortFields, Timeout).`` where:
* ``Query`` is written respecting the [Lucene Query Syntax](http://lucene.apache.org/core/3_6_0/queryparsersyntax.html)
* ``PageSize`` is the number of results per page you expect.
* ``SortFields`` is a list of atoms that will determine the result sort order for equally scored results
* ``Timeout`` is the number of milliseconds to wait for a return. If no ``Timeout`` is specified, it defaults to 5000. Not to have a timeout, you should use the atom ``infinity``.
Both functions may return:
* the atom ``timeout`` if it took more than ``Timeout`` milliseconds to find the desired docs
* the first page of results together with metadata as described below

### Results
A results page looks like ``{Docs::[lucene:doc()], Data::lucene:metadata()}`` where:
* ``Docs`` is a list of no more than ``PageSize`` documents that match the ``Query``
* ``Data`` is a ``proplist`` that include the following fields:
  * ``next_page``: The token used to retrieve the following page (see below), if present
  * ``total_hits``: How many documents match the query across all pages
  * ``first_hit``: Which is the position of the first returned doc in the whole set of docs that match the query (e.g. if ``PageSize == 5``, for the first page ``first_hit == 1``; for the page #2, ``first_hit == 6``; etc.)

### Paging
To get the following page use: ``lucene:continue(PageToken, PageSize).`` or ``lucene:continue(PageToken, PageSize, Timeout).`` where ``PageToken`` comes from the metadata of the previous page and the rest of the parameters and results have the same types, format and meaning as in ``lucene:match/2`` or ``lucene:match/4`` functions.

## Special Data Types
Besides what Lucene already offers, *Lucene Server* provides support for indexing and querying some extra data types:

### Atoms
Atoms are treated as strings: You may add them as values in a document and query them using standard _Lucene Query Syntax_ for strings

### Numbers
*Lucene Server* lets you store integers and floats and then use them in range queries (i.e. ``<Field>:[<Min> TO <Max>]``) properly, respecting the field's data type instead of treating them as strings as Lucene does by default.

### Geo
*Lucene Server* provides support for managing and querying geo-spatial coordinates (i.e. latitude and longitude pairs).
* To construct a ``lucene:geo()`` object, use: ``lucene_utils:geo(Lat, Lng)`` where ``Lat`` and ``Lng`` are floating point numbers
* You can then use it as a value on a ``lucene:doc()``
* To find documents near a certain point, include the following term in your query: ``<Field>.near:<Lat>,<Lng>,<Miles>``. That query will filter documents within a ``<Miles>`` radio of ``<Lat>,<Lng>`` and also will rank results according to that distances (with closer docs ranking higher).

## Special Queries
### Calling an Erlang function
In the same way you can write ".near" queries, you can also write ".erlang" ones. The syntax is ``<Field>.erlang:<Module>:<Function>[:<Args>]``.
The function ``Module:Function`` is expected to comply with the following spec:
* If no args are provided: ``-spec Mod:Fun([term()]) -> [false | float()].``
* If Args are provided (and they should be written as a list): ``-spec Mod:Fun(type_of_arg1(), type_of_arg2(),... [term()]) -> [false | float()].``

The function will be called with the list of values for field ``Field`` and it is expected to return a list of results with the same length of the one received. For each element in the original list, the function may return (in the same place of the new list):
* ``false`` if it's not a match
* a ``float()`` representing the score of such a document