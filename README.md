# Freyr

[![Build Status](https://travis-ci.org/cloud8421/freyr.svg?branch=master)](https://travis-ci.org/cloud8421/freyr)

Proof of concept application to monitor plant life.

## Components

- Arduino-based sensor and visual feedback (not done yet :pensive:);
- Server component to process data, send notifications, expose an api (Erlang).

## Installation

Assuming you have [Rebar](https://github.com/rebar/rebar) installed:

    $ make deps
    $ make compile

## Running tests

Unit tests are available in `/test`, while integration tests are available in `/itest`, respectively available
with the following commands:

    $ make test
    $ make ct

## Running the application

You can start the app by running `make start` and then type once inside the erlang shell):

    (user@host)1> freyr_app:start([], []).

At this point, the listening tcp socket (to receive data from the Arduino) will be available at `localhost:5678`.

The json api will exposed at `http://localhost:9000`. Try <http://localhost:9000/readings>.

## Sample data

You can run the `tester.rb` script to send some sample data:

    $ ruby tester.rb

It should work with any Ruby >= 1.9.

## Release build

Not available yet.

