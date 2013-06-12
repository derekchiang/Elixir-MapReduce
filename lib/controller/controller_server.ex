defmodule Controller.Server do
  use GenServer.Behaviour

  @doc """
  A record for storing the state of the controller
  """
  defrecord ControllerState,
    # the module that defines map() and reduce()
    module: nil,
    # the keys to mapper_input_tbl
    mapper_input_keys: [],
    # the keys to reducer_input_tbl
    reducer_input_keys: [],
    # the total number of mappers
    num_mappers: nil,
    # the total number of reducers
    num_reducers: nil

  @doc """
  Convenience function to start the controller gen_server
  """
  def start_link(path) do
    :gen_server.start_link({:local, :controller}, __MODULE__, path, [])
  end

  @doc """
  Init function.
  """
  def init(path) do
    num_mappers = MapReduce.Config.num_mappers
    num_reducers = MapReduce.Config.num_reducers
    {:ok, ControllerState.new(num_mappers: num_mappers,
                              num_reducers: num_reducers)}
  end

  @doc """
  Handle {:start, module_path, input_path}.
  
  `module_path` is the path to an .ex or .erl file which defines a
  module with at least two functions: `map(key, value)` and
  `reduce(key, values)`.  You may optionally define a function
  `combine(key_value_pairs)`, which will be used to combine all
  key-value pairs emitted by mappers and combine them into a
  list of {key, values}.

  `input_path` is the path to a directory which contains a set of
  input files.  All files under this directory will eventually be
  read 
  """
  def handle_cast({:start, module_path, input_path}, state) do
    # Load module and get a list of input files
    [{module, _}] = Code.load_file(module_path)
    {:ok, filenames} = :file.list_dir(input_path)
    
    filenames = lc fname inlist filenames do
      # absolute path to the input file
      # TODO: what if input_path doesn't end with '/'?
      input_path <> fname
    end

    state = state.module(module)
    state = state.mapper_input_keys(filenames)

    # Start the mapper supervision tree
    {:ok, supervisor_pid} = Mapper.Supervisor.start_link(length(filenames))
    true = :erlang.register(:mapper_supervisor, supervisor_pid)

    {:noreply, state}
  end

  def handle_cast({:mapper_ready, pid}, state) do
    if state.mapper_input_keys == [] do
      {:noreply, state}
    else
      [fname | rest] = state.mapper_input_keys
      :gen_server.cast(pid, {:key, fname})
      {:noreply, state.mapper_input_keys(rest)}
    end
  end

  def handle_cast(:mapper_done, state) do
    new_state = state.update_num_mappers_done(&1 + 1)
    if new_state.num_mappers_done == new_state.num_mappers do
      # All map jobs are down, now we do:
      # 1. shut down the mapper supervision tree
      # 2. combine results returned by mappers
      # 3. split the combine results and store them in ets
      # 4. start the reducer supervision tree
      true = :erlang.exit(:mapper_supervisor, :shutdown)
      new_state = new_state.reducer_input_keys(combine_and_split(new_state.num_reducers))
      {:ok, supervisor_pid} = Reducer.Supervisor.start_link(new_state.num_mappers)
      true = :erlang.register(:reducer_supervisor, supervisor_pid)
    end
    {:noreply, new_state}
  end

  def handle_cast({:reducer_ready, pid}, state) do
    if state.reducer_input_keys == [] do
      {:noreply, state}
    else
      [k | rest] = state.reducer_input_keys
      :gen_server.cast(pid, {:key, k})
      {:noreply, state.reducer_input_keys(rest)}
    end
  end

  def handle_cast(:reducer_done, state) do
    new_state = state.update_num_reducers_done(&1 + 1)
    if new_state.num_reducers_done == new_state.num_reducers do
      # All reduce jobs are down, now we do:
      # 1. shut down the reducer supervision tree
      true = :erlang.exit(:reducer_supervisor, :shutdown)
    end
    {:noreply, new_state}
  end

  def combine_and_split(num_reducers) do
    lst = List.flatten(:ets.lookup(:mapper_table, :output))
    dict = HashDict.new
    List.foldl(lst, dict, fn({key, value}, acc) ->
      HashDict.update(acc, key, [value], [&1 | value])
    end)
    combined_lst = HashDict.to_list(dict)
    
    # Split the combined results into the number of reducers
    split_to_multiple(combined_lst, num_reducers)
  end

  defp split_to_multiple(lst, n) when n == 0 or n == 1 do
    :ets.insert(:reducer_input_tbl, {n, lst})
    [n]
  end

  defp split_to_multiple(lst, n) do
    len = round(length(lst) / n)
    {sub_lst, lst} = Enum.split(lst, len)
    :ets.insert(:reducer_input_tbl, {n, sub_lst})
    [n | split_to_multiple(lst, n-1)]
  end
end