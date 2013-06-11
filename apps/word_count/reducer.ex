defmodule WordCount.Reducer do
  def start_link(args) do
    {:ok, spawn_link(__MODULE__, :run, [args])}
  end

  def run(key) do
    values = :ets.lookup(:reducer_table, key)
    sum = List.foldl(values, 0, fn(x, acc) -> acc + x end)
    IO.puts "hahaha"
    :ets.insert(:reducer_table, {:reducer_output, [{key, sum}]})
    :gen_server.cast(:controller, :reducer_done)
  end
end