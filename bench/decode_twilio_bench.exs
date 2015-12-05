defmodule Twilio do
  use Benchfella

  @sms """
  {
    "sid": "MMc781610ec0b3400c9e0cab8e757da937",
    "date_created": "Mon, 19 Oct 2015 07:07:03 +0000",
    "date_updated": "Mon, 19 Oct 2015 07:07:03 +0000",
    "date_sent": null,
    "account_sid": "AC5ef872f6da5a21de157d80997a64bd33",
    "to": "+16518675309",
    "from": "+14158141829",
    "body": "Hey Jenny! Good luck on the bar exam!",
    "status": "queued",
    "num_segments": "1",
    "num_media": "1",
    "direction": "outbound-api",
    "api_version": "2010-04-01",
    "price": null,
    "price_unit": "USD",
    "error_code": null,
    "error_message": null,
    "uri": "/2010-04-01/Accounts/AC5ef872f6da5a21de157d80997a64bd33/Messages/MMc781610ec0b3400c9e0cab8e757da937.json",
    "subresource_uris": {
      "media": "/2010-04-01/Accounts/AC5ef872f6da5a21de157d80997a64bd33/Messages/MMc781610ec0b3400c9e0cab8e757da937/Media.json"
    }
  }
  """

  bench "jsx decode twilio" do
    :jsx.decode @sms
  end
end