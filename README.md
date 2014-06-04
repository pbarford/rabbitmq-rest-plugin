# RabbitMQ Restful Adapter Plugin #

This plugin provides a rest adapter that clients can call to post messages to the rabbit cluster.

The call to the endpoint results in the body of the request along with a subset of the headers to be added in a message which is then dispatched to an exchange on the server. The application can then bind a queue to the exchange if they wish to process.


# Usage #

The following are configurable in the application config -> rabbitmq_rest.

- http_uri -> allows you to configure the uri of the rest endpoint, defaults to __/restmq__.
- http_port -> allows you to configure the port that the rest endpoint is listening on, defaults to __4040__.
- http_listener_count -> allows you to configure the number of listeners, default to __5__.
- exchange_name -> allows you to configure the name of the exchange that gets created on startup, defaults to __restInbound__.
- msg_ttl -> allows you to configure the expiration on the message, defaults to __5000 m/s__.


Endpoint only accepts POST requests.




