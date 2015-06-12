defmodule JSX.Mixfile do
use Mix.Project

  def project do
    [
      app: :jsx,
      version: "2.6.2",
      description: "an erlang application for consuming, producing and manipulating json. inspired by yajl",
      deps: deps(Mix.env),
      package: package,
      language: :erlang,
      erlc_options: opts(Mix.env)
    ]
  end

  defp opts(:dev), do: [d: :TEST] ++ opts(:prod)
  defp opts(_), do: [d: :maps_support, d: :maps_always]

  defp deps(_), do: [{:mixunit, git: "git@github.com:talentdeficit/mixunit.git", only: :dev}]

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

