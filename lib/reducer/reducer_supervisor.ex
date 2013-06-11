defmodule Reducer.Supervisor do
  use Supervisor.Behaviour

  def start_link(args) do
    :supervisor.start_link(__MODULE__, args)
  end

  # combined_results should be in the form of [{key, value_list}]
  def init({path, combined_results}) do
    :ets.new(:reducer_table, [:duplicate_bag, :named_table,
                             {:read_concurrency, true},
                             {:write_concurrency, true}])
    [module] = Code.load_file(path <> "/reducer.ex")
    workers = lc {key, values} inlist combined_results do
      :etc.insert(:reducer_table, {key, values})
      worker(module, [key])
    end
    :gen_server.cast(:controller, {:num_reducers, length(workers)})
    supervise workers, strategy: :one_for_one
  end
end