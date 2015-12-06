defmodule Petstore.Encode.Map do
  use Benchfella

  @pet %{
    "category" => %{"id" => 0, "name" => "string"},
    "id" => 0,
    "name" => "doggie",
    "photoUrls" => ["string"],
    "status" => "available",
    "tags" => [%{"id" => 0, "name" => "string"}]
  }
  
  bench "map     :: petstore" do
    :jsx.encode @pet
  end
end