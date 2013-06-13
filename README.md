# Elixir MapReduce

A light-weight, high-performance MapReduce framework written in Elixir, for Erlang VM developers.

The project is under active development.  Do NOT use it right now.

# Usage

TODO

# Architecture

As described in the [original MapReduce paper](http://research.google.com/archive/mapreduce.html), mappers take input of the form `(key, value)` and produce output of the form `list((key, value))`; reducers take input of the form `(key, list(value))`, and produce output of the form `list(value)`.

As a user of Elixir MapReduce (EMR), you may write your custom `map()`, `combine()`, and `reduce()` functions, but only `map()` and `reduce()` are required.  By default, `combine()` aggregates `(key, value)` pairs with the same key into `(key, list(value))`.  For example, `[(1, 'a'), (1, 'b')]` will be combined into (1, ['a', 'b']).

Elixir MapReduce, abbreviated as EMR, consists of three components: controller, mapper, and reducer.  Each of these components implements the `gen_server` behaviour and has its own supervisor.

A typically workflow looks like this:

1. The controller receive a message {:start, path_to_module, path_to_input_files}.  The module specified by the path needs to have at least a `map()` and a `reduce()` functions defined.

2. The controller then loads the module and gets a list of file names from the path to input files.

3. The controller thenstarts a supervisor, which in turn starts a group of mappers, the exact number of which can be specified in a config file.

4. When a mapper is started up, it sends a message to the controller, indicating that it's ready.

5. When the controller receives a message from a mapper, it replies with a filename.

6. When a mapper gets a filename, it reads the file and start the user-defined `map()` function with the argument `(filename, file content)`.

7. When a mapper is done (`map()` returns), it writes the returned value of `map()` on disk, in a specified location.  It then sends a message to controller, indicating that it's done.

8. When the controller receives messages from all controllers indicating they are done, it shuts down the mappers' supervisor (and therefore shuts down all mappers).

9. The controller then combines output from mappers.  It then splits the combined output into several files so that it can later distribute them to reducers.

10. The controller then starts another supervisor, which in turn starts a group of reducers.

11. When a reducer is started up, it sends a message to the controller, indicating that it's ready.

12. The controller then replies with the file path to a spllitted file.

13. The reducer reads the file and unmarshal the content to a `(key, list(values))` pair.  It then starts the user-defined `reduce()` function.

14. When `reduce()` returns, the reducer writes the output to disk and sends a message to the controller, indicating that it's done.

15. After the controller receives messages from all reducers, it shuts down the reducer supervision tree.

16. Then the controller sits idlely, waiting for another task.