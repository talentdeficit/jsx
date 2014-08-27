defmodule JSX.Mixfile do
use Mix.Project

  def project do
    [
      app: :jsx,
      version: "2.1.1",
      description: "an erlang application for consuming, producing and manipulating json. inspired by yajl",
      package: package
    ]
  end

  defp package do
    [
      files: [
        "CHANGES.md",
        "LICENSE",
        "package.exs",
        "README.md",
        "rebar.config",
        "rebar.config.script",
        "config",
        "src"
      ],
      contributors: ["alisdair sullivan"],
      links: [{"github", "https://github.com/talentdeficit/jsx"}]
    ]
  end
end