defmodule Petstore do
  use Benchfella

  @pet """
  {
    "id": 0,
    "category": {
      "id": 0,
      "name": "string"
    },
    "name": "doggie",
    "photoUrls": [
      "string"
    ],
    "tags": [
      {
        "id": 0,
        "name": "string"
      }
    ],
    "status": "available"
  }
  """

  bench "decode  :: petstore" do
    :jsx.decode @pet
  end

  bench "to_list :: petstore" do
    :jsx.to_list @pet
  end

  bench "is_json :: petstore" do
    :jsx.is_json @pet
  end
end