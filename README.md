WOW-stream
----------

WOW-stream is just a toy project in order to improve my haskell knowledge. The application is a server that listens for websocket connections from clients. After connecting successfully, clients can listen to a twitter streams which is provided by the server. Additionally each client can filter the stream, talk to other connected clients via messages or request a list of all connected clients.

The application is not really useful by itself. I was just aiming to use and connect different technologies like Streaming Data (Twitter Stream - Http Long Polling) and Websockets. I decided to use Polysemy for application architecture in order to get a better understanding of it.