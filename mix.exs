defmodule JSX.Mixfile do
use Mix.Project

  def project do
    [
      app: :jsx,
      version: "2.6.0",
      description: "an erlang application for consuming, producing and manipulating json. inspired by yajl",
      deps: deps(Mix.env),
      package: package,
      language: :erlang,
      erlc_options: opts(Mix.env)
    ]
  end

  defp opts(:dev), do: [d: :TEST]
  defp opts(_), do: []

  defp deps(), do: [{:mixunit, "~> 0.9.1", [only: :dev]}]

  defp package do
    [
      files: [
        "CHANGES.md",
        "LICENSE",
        "mix.exs",
        "rebar.config",
        "README.md",
        "src"
      ],
      contributors: ["alisdair sullivan"],
      links: %{"github" => "https://github.com/talentdeficit/jsx"},
      licenses: ["MIT"]
    ]
  end
end

