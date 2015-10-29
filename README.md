# Taps
A library of conveniences for working with SERIES

## Introduction

Richard Waters' SERIES package for Common Lisp provides a powerful and interesting solution to working with sequences, collections, and streams. The **Taps** library adds a small number of functions that make SERIES more convenient to work with in many common situations.

A **tap** in the real world is a device that opens and closes a valve, enabling a fluid to flow or preventing it from doing so. In the Taps library, a tap is a function that turns some data structure into a series of values. Taps provides functions that create series from streams, strings, and iterative computations, and provides convenient functions for mapping over, filtering, and collecting results.

The workflow for which Taps is designed is this:

1. Call a function to tap a data structure, creating a flow of values
2. Select or filter the values of interest using another function
3. Map another function over the selected values to produce a series of outputs
4. Collect the results using a collector function

Used judiciously, Taps can make many looping or iterative processes easier to write and easier to read.

## Taps Reference

**+line-break-characters+** *Special variable*<br/>
The default set of characters used to break lines of text.

**+whitespace-characters+** *Special variable*<br/>
The default set of characters treated as whitespace.

**drop** *Generic function*<br/>

    drop (n series) => series*

Returns a new series containing all the elements of `series` except for the first `n` elements.

**filter** *Function*<br/>

    filter (fn series) => series*
Returns a new series, `series*` created by mapping `fn` over the elements of `series` and collecting those for which `fn` returns a true value.

**take** *Generic function*<br/>

    take (n series) => series*
Returns a new series that contains the first `n` elements of `series`.

**take-by** *Generic function*<br/>

    take-by (n series) => series*
Returns a new series created by splitting `series` into shorter series, each one containing `n` elements of `series`.

**take-m-by-n** *Generic function*<br/>

    take-m-by-n (m n series) => series*
Returns a new series created by repeatedly collecting `m` elements of `series`, starting with the first element of `series` and moving the starting point forward each time by `n`.

**take-until** *Function*<br/>

    take-until (predicate series) => series*
Returns a new series created by collecting elements from the start of `series` until applying `predicate` to an element returns a true value.

**tap** *Generic function*<br/>

    tap (element-type source &key &allow-other-keys) => series

Returns a new series created by computing a (possibly infinite) series of values from `source`. The type of values and the method used to compute them depends on both `element-type` and `source`.

| element-type | source | Description |  
|  ------	| ------	| ------	|  
| `:bytes` | `stream` | Returns a series of bytes read from the stream. |  
| `:characters` | `stream` | Returns a series of characters read from the stream. |  
| `:characters` | `string` | Returns a series of characters read from the string. |  
| `:words` | `stream` | Returns a series of 'words' read from the stream. <br/>Pass the keyword argument `:word-break-characters` <br/>with a list of characters to control text is broken into words. |  
| `:words` | `string` | Returns a series of 'words' read from the string. <br/>Pass the keyword argument `:word-break-characters` <br/>with a list of characters to control text is broken into words. |  
| `:lines` | `stream` | Returns a series of lines of text read from the stream.  <br/>Pass the keyword argument `:line-break-characters` <br/>with a list of characters to control how text is broken into lines.|  
| `:lines` | `string` | Returns a series of lines of text read from the string.  <br/>Pass the keyword argument `:line-break-characters` <br/>with a list of characters to control how text is broken into lines.|  
| `:objects` | `stream` | Returns a series of Lisp objects read from the stream. |  
| `:objects` | `string` | Returns a series of Lisp objects read from the string. |  
| `:hash-entries` | `hash-table` | Returns two series as multiple values: a series of the keys in the hash-table, and a series of its corresponding values. |  
| `:keys` | `hash-table` | Returns a series containing the keys from the hash-table. |  
| `:values` | `hash-table` | Returns a series containing the values from the hash-table. |  

**tap-fn** *Function*<br/>

    tap-fn (fn series) => series

Returns a new series of values computed by mapping the function `fn` over the elements of `series`.

**tap-integers** *Function*<br/>

    tap-integers (&key from by upto below downto above) => series

Returns a new series of integers. The first element of the series is
`from`, whose default value is zero. Each successive element is
obtained by adding the value of `by` to the previous one. THe default
value of `by` is one. If `upto` is supplied then the series continues
until the next element is greater than `upto`. If `below` is supplied,
it continues until the next element is greater than or equal to
`below`. If `downto` is supplied, it continues until the next element
is less than `downto`. If `above` is supplied then it continues until
the next element is less than or equal to `above`.

The series returned by tap-integers may be of infinite length. For
example, the expression

    (tap-integers)

returns an infinite series that starts with

    #Z(0 1 2 3 ...

**tap-random-integers** *Function*<br/>

    tap-random-integers (below &optional (random-state *random-state*)) => series

Returns an infinite series of random integers, each less than `below`.

