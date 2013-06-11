defmodule MapReduce.Mixfile do
  use Mix.Project

  def project do
    [ app: :map_reduce,
      version: "0.0.1",
      deps: deps ]
  end

  # Configuration for the OTP application
  def application do
    [mod: { MapReduce, ["/media/Work_Study/CS/workspace/elixir/map_reduce/apps/word_count"] },
     registered: [:controller, :binary_table]]
  end

  # Returns the list of dependencies in the format:
  # { :foobar, "0.1", git: "https://github.com/elixir-lang/foobar.git" }
  defp deps do
    []
  end
end
