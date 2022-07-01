WOW-stream
----------

WOW-stream is just a toy project in order to improve my haskell knowledge. The application has two responsibilities. It connects to the Twitter API and listens for tweets from the sample stream. It acts as a server that listens for websocket connections from clients. Successfully connected clients can listen to the provided twitter stream and apply an individual filter to it. Additionally each client can talk to other connected clients via simple messages or request a list of all connected clients.

The application does not have a real purpose. I was just aiming to use and connect different technologies like Streaming Data (Twitter Stream - Http Long Polling) and Websockets. I decided to use Polysemy for application architecture in order to get a better understanding of how it works.
