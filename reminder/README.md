# Reminder Application

- Add an event.
- Show a warning when the event is about to start.
- Cancel an event by name.
- No persistent disk storage.
- Hot reload.
- Interaction is done via CLI, but should be extensible to other interfaces.

## Structure

![structure-of-reminder-program](https://learnyousomeerlang.com/static/img/reminder-structure.png)

### Event Server

- Accepts subscriptions from clients
- Forwards notifications from event processes to each of each subscribers
- Accepts messages to add events
- Can accept messages to cancel an event and subsequently kill the event process
- Can be terminated by a client
- Can have its code reload via the shell

## Client

- Subscribes to the event server and receives notifications as messages
- Asks the server to add an event with all its details
- Asks the server to cancel an event
- Monitors the server (to know if it goes down)
- Shuts down the event server if needed

## x, y and z

- Represent a notification waiting to fire (they're basically just timers linked to the event server)
- Send a message to the event server when the time is up
- Receive a cancellation message and die

Note that all clients (IM, mail, etc.) are notified about all events, and a cancellation is not something to warn the clients about.

Here's a more complex graph with all the possible messages:

![more-complex-graph](https://learnyousomeerlang.com/static/img/reminder-bubbles-and-arrows.png)

It should be noted that using one process per event to be reminded of is likely going to be overkill and hard to scale in a real world application. However, for an application you are going to be the sole user of, this is good enough. A different approach could be using functions such as [timer:send_after/2-3](http://erldocs.com/18.0/stdlib/timer.html#send_after/2) to avoid spawning too many processes.

## How to compile and run the erlang shell with the application context

The files should all be compiled and put inside the `ebin/` directory.

```bash
erl -make
```

And then start the application.

```bash
erl -pa ebin/
```

Or, start the Erlang shell and compile the files manually.

```bash
erl
% make:all([load]).
```

This will look for a file named 'Emakefile' in the current directory, recompile it(if it changed) and load the new files.

## References

<https://learnyousomeerlang.com/designing-a-concurrent-application>
