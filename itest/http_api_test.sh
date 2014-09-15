#!/usr/bin/env bats

@test "200 for /devices/:id with existing device" {
  [ "$(curl 2>/dev/null -i http://localhost:9000/devices/abc1234xyz | grep 200)" ]
}

@test "404 for /devices/:id with non existing device" {
  [ "$(curl 2>/dev/null -i http://localhost:9000/devices/foo | grep 404)" ]
}

@test "200 for /devices/:id/readings with existing device" {
  [ "$(curl 2>/dev/null -i http://localhost:9000/devices/abc1234xyz/readings | grep 200)" ]
}

@test "404 for /devices/:id/readings with non existing device" {
  [ "$(curl 2>/dev/null -i http://localhost:9000/devices/foo/readings | grep 404)" ]
}
