# rabbitmq-rest-plugin #

This plugin provides a rest adapter that clients can call to post messages to the rabbit cluster.

The call to the endpoint results in the body of the request along with a subset of the headers to be added in a message which is then dispatched to an exchange on the server. The application can then bind a queue to the exchange if they wish to process.


# Usage #

The following are configurable in the application config -> rabbitmq_rest.

- http_uri -> allows you to configure the uri of the rest endpoint, defaults to "/restmq".
- http_port -> allows you to configure the port that the rest endpoint is listening on, defaults to "4040".
- http_listener_count -> allows you to configure the number of listeners, default to "5".
- exchange_name -> allows you to configure the name of the exchange that gets created on startup, defaults to "restInbound".
- msg_ttl -> allows you to configure the expiration on the message, defaults to 5000 m/s.


Endpoint only accepts POST requests.




