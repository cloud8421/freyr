require 'socket'

temperature = [21.2, 22.5, 20.8, 19.5, 24.2, 23.7, 21.7]
brightness  = [978,  850,  400,  390,  750,  650,  722]
moisture    = [3500, 4000, 2200, 3700, 2800, 3421, 2738]
device_id   = "abc1234xyz"

TCPSocket.open 'localhost', 5678 do |s|
  10.times do |n|
    msg = "device:#{device_id}|temperature:f:#{temperature.sample}|brightness:i:#{brightness.sample}|moisture:i:#{moisture.sample}"
    s.send msg, 0
    sleep 0.3
  end
end
