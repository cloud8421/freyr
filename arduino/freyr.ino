#include <SPI.h>
#include <Ethernet.h>
#include <stdlib.h>

//Sensors location
const int temperaturePin = A0;
const int brightnessPin = A1;
const int moisturePin = A2;
const float baselineTemp = 20.0;
const String deviceId = "3711af52-3856-11e4-863c-b8e8563a72e8";

//Brightness baseline
int brightnessValue;
int brightnessLow = 1023;
int brightnessHigh = 0;

//Ethernet setup
byte mac[] = { 0xDE, 0xAD, 0xBE, 0xEF, 0xFE, 0xED };
byte server[] = { 10, 2, 20, 43 };
EthernetClient client;

void setup() {
  Serial.begin(57600);
  Ethernet.begin(mac);

  Serial.println("connecting...");

  //Connect to tcp server
  if (client.connect(server, 5678)) {
    Serial.println("connected");
  } else {
    Serial.println("connection failed");
  }

  //Calibrate brightness sensor
  while (millis() < 500) {
    brightnessValue = analogRead(brightnessPin);

    if (brightnessValue > brightnessHigh) {
      brightnessHigh = brightnessValue;
    }
    if (brightnessValue < brightnessLow) {
      brightnessLow = brightnessValue;
    }
  }
}

void loop() {
  int temperatureValue = analogRead(temperaturePin);
  brightnessValue = analogRead(brightnessPin);

  float voltage = (temperatureValue/1024.0) * 5.0;
  float temperature = (voltage - 0.5) * 100;

  int moistureValue = analogRead(moisturePin);

  char tmp[4];
  String temperatureStr = dtostrf(temperature,1,2,tmp);
  String brightnessStr = String(brightnessValue);
  String moistureStr = String(moistureValue);

  String dataPoint = "device:" + deviceId + "|temperature:f:" + temperatureStr + "|brightness:i:" + brightnessStr + "|moisture:i:" + moistureStr;

  //Data sent as an array to avoid incomplete packets  
  int n = dataPoint.length()+1;
  char st[n];
  dataPoint.toCharArray(st,n);
  client.print(st);

  //Send data every 5 minutes
  delay(3000000);
}
