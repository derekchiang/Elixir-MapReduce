defmodule Mapper.Supervisor do
  use Supervisor.Behaviour

  def start_link(args) do
    :supervisor.start_link(__MODULE__, args)
  end

  def init(path) do
    # Create ets tables
    :ets.new(:binary_table, [:set, :named_table,
                            {:read_concurrency, true},
                            {:write_concurrency, true}])
    :ets.new(:mapper_table, [:duplicate_bag, :named_table,
                            {:read_concurrency, true},
                            {:write_concurrency, true}])

    [{module, _}] = Code.load_file(path <> "/mapper.ex")
    {:ok, filenames} = :file.list_dir(path <> "/input")
    workers = lc filename inlist filenames do 
      {:ok, binary} = :file.read_file(path <> "/input/" <> to_binary(filename))
      :ets.insert(:binary_table, {filename, binary})
      worker(module, [filename])
    end
    :gen_server.cast(:controller, {:num_mappers, length(workers)})
    supervise workers, strategy: :one_for_one
  end
end