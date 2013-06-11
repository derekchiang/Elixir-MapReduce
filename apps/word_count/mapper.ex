defmodule WordCount.Mapper do
  def start_link(args) do
    :proc_lib.start_link(__MODULE__, :init, [args, self])
  end

  def init(filename, parent) do
    deb = :sys.debug_options([])
    :proc_lib.init_ack(parent, {:ok, self})
    run(filename, parent, deb)
  end

  def run(filename, parent, deb) do
    [{_, file}] = :ets.lookup(:binary_table, filename)
    words = String.split(file)
    pairs = lc word inlist words do
      {word, 1}
    end
    :ets.insert(:mapper_table, {:mapper_output, pairs})
    :gen_server.cast(:controller, :mapper_done)
  end
end