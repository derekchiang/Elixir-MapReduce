defmodule Controller.Server do
  use GenServer.Behaviour

  defrecord ControllerState,
    phrase: :map,
    lst: [],
    path: nil,
    num_mappers: nil,
    num_reducers: nil,
    num_mappers_done: 0,
    num_reducers_done: 0

  def start_link(path) do
    :gen_server.start_link({:local, :controller}, __MODULE__, path, [])
  end

  def init(path) do
    # Start mappers
    Mapper.Supervisor.start_link(path)
    # State is in the form of {:phrase(map or combine), lst, number of workers done}
    state = ControllerState.new(path: path)
    {:ok, state}
  end

  def handle_cast({:num_mappers, num_mappers}, state) do
    new_state = state.num_mappers(num_mappers)
    {:noreply, new_state}
  end

  def handle_cast({:num_reducers, num_reducers}, state) do
    new_state = state.num_reducers(num_reducers)
    {:noreply, new_state}
  end

  def handle_cast(:mapper_done, state) do
    new_state = state.update_num_mappers_done(&1 + 1)
    if new_state.num_mappers_done == new_state.num_mappers do
      combined_results = combine(:lists.flatten(:ets.lookup(:mapper_table, :mapper_output)))
      Reducer.Supervisor.start_link({new_state.path, combined_results})
      {:noreply, new_state} # reset lst
    else
      {:noreply, new_state}
    end
  end

  def handle_cast(:reducer_done, state) do
    new_state = state.update_num_mappers_done(&1 + 1)
    if new_state.num_reducers_done == new_state.num_reducers do
      :file.write_file(new_state.path <> "/output.txt", to_binary(:ets.lookup(:reducer_table, :reducer_output)))
      {:stop, :done, new_state}
    else
      {:noreply, new_state}
    end
  end

  def combine(lst) do
    dict = HashDict.new
    List.foldl(lst, dict, fn({key, value}, acc) ->
      HashDict.update(acc, key, [value], [&1 | value])
    end)
    HashDict.to_list(dict)
  end
end