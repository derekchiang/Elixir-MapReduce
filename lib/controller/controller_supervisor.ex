defmodule Controller.Supervisor do
  use Supervisor.Behaviour

  def start_link(args) do
    :supervisor.start_link(__MODULE__, args)
  end

  def init([]) do
    children = [
      # Define workers and child supervisors to be supervised
      # worker(MapReduce.Worker, [])
    ]

    # See http://elixir-lang.org/docs/stable/Supervisor.Behaviour.html
    # for other strategies and supported options
    supervise(children, strategy: :one_for_one)
  end

  def init([path]) do
    children = [ worker(Controller.Server, [path]) ]
    supervise children, strategy: :one_for_one
  end
end
