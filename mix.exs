defmodule Namex.MixProject do
  use Mix.Project

  def project do
    [
      app: :namex,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps(),
      description: "A name parser",
      package: [
        licenses: ["MIT"],
        maintainers: ["Trevor Fenn<sgtpepper43@gmail.com>"],
        links: %{"GitHub" => "https://github.com/sgtpepper43/namex"},
        files: ["lib", "mix.exs", "README*", "LICENSE*", "src"]
      ],
      source_url: "https://github.com/sgtpepper43/namex"
    ]
  end

  def application do
    [
      extra_applications: [:logger]
    ]
  end

  defp deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev}
    ]
  end
end
