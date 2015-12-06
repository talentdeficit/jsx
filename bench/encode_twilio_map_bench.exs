defmodule Twilio.Encode.Map do
  use Benchfella

  @sms %{
    "account_sid" => "AC5ef872f6da5a21de157d80997a64bd33",
    "api_version" => "2010-04-01",
    "body" => "Hey Jenny! Good luck on the bar exam!",
    "date_created" => "Mon, 19 Oct 2015 07:07:03 +0000",
    "date_sent" => :null,
    "date_updated" => "Mon, 19 Oct 2015 07:07:03 +0000",
    "direction" => "outbound-api",
    "error_code" => :null,
    "error_message" => :null,
    "from" => "+14158141829",
    "num_media" => "1",
    "num_segments" => "1",
    "price" => :null,
    "price_unit" => "USD",
    "sid" => "MMc781610ec0b3400c9e0cab8e757da937",
    "status" => "queued",
    "subresource_uris" => %{
      "media" => "/2010-04-01/Accounts/AC5ef872f6da5a21de157d80997a64bd33/Messages/MMc781610ec0b3400c9e0cab8e757da937/Media.json"
    },
    "to" => "+16518675309",
    "uri" => "/2010-04-01/Accounts/AC5ef872f6da5a21de157d80997a64bd33/Messages/MMc781610ec0b3400c9e0cab8e757da937.json"
  }

  bench "map     :: twilio" do
    :jsx.encode @sms
  end
end