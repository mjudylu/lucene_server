**Lucene Server** is an *Erlang* application that let's you manage and query documents using an **in-memory** [Lucene](http://lucene.apache.org) backend

## Usage
To start the application just run ``lucene_server:start().``

## Adding/Deleting documents
To add documents use: ``lucene:add(Docs).`` where ``Docs :: [lucene:doc()]``
Each document is a ``proplist`` where the keys can be atoms, strings or binaries and the values can be numbers, atoms, strings or ``lucene:geo()``.
To delete documents use: ``lucene:del(Query).`` where ``Query`` is written respecting the [Lucene Query Syntax](http://lucene.apache.org/core/3_6_0/queryparsersyntax.html).

## Querying
To find documents according to a query use: ``lucene:match(Query, PageSize).`` or ``lucene:match(Query, PageSize, Timeout).`` where ``Query`` is written respecting the [Lucene Query Syntax](http://lucene.apache.org/core/3_6_0/queryparsersyntax.html) and ``PageSize`` is the number of results per page you expect.
If no ``Timeout`` is specified, it defaults to 5000.
Both functions may return:
* the atom ``'$end_of_table'`` if there're no results matching the query.
* the atom ``timeout`` if it took more than ``Timeout`` milliseconds to find the desired docs
* the first page of results together with metadata as described below

### Results
A results page looks like ``{Docs::[lucene:doc()], Data::lucene:metadata()}`` where:
* ``Docs`` is a list of no more than ``PageSize`` documents that match the ``Query``
* ``Data`` is a ``proplist`` that include the following fields:
  * ``page_token``: The token used to retrieve the following page (see below)
  * ``total_hits``: How many documents match the query across all pages
  * ``first_hit``: Which is the position of the first returned doc in the whole set of docs that match the query (e.g. if ``PageSize == 5``, for the first page ``first_hit == 1``; for the page #2, ``first_hit == 6``; etc.)

### Paging
To get the following page use: ``lucene:continue(PageToken, PageSize).`` or ``lucene:continue(PageToken, PageSize, Timeout).`` where ``PageToken`` comes from the metadata of the previous page and the rest of the parameters and results have the same types, format and meaning as in ``lucene:match/2`` or ``lucene:match/3`` functions.

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