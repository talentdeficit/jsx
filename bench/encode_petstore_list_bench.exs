defmodule Petstore.Encode.List do
  use Benchfella

  @pet [
    {"id", 0},
    {"category", [{"id", 0}, {"name", "string"}]},
    {"name", "doggie"},
    {"photoUrls", ["string"]},
    {"tags", [[{"id", 0}, {"name", "string"}]]},
    {"status", "available"}
  ]
  
  bench "list    :: petstore" do
    :jsx.encode @pet
  end
end