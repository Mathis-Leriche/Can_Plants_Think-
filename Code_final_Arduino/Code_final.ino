int pins[] = {0, 2, 3, 5, 6, 8, 9, 7};
unsigned long time_period = 7200000;//7200000
unsigned long time_night = 36000000;//36000000
unsigned long nb_of_days_East = 4;
unsigned long nb_of_days_West = 3;
static unsigned long lastTime = -7200000;
static int day = 1;
static int light = 1;
static bool isNight = false;

void setup() {
  Serial.begin(9600);
  for (int i = 1; i < 8; i++) {
    pinMode(pins[i], OUTPUT);
  }
}

void loop() {

  if (millis() - lastTime >= (isNight ? time_night : time_period)) {
    lastTime = millis();
    if (!isNight) {
      light = (day <= nb_of_days_East) ? light + 1 : light - 1;
      if (light < 1 || light >= 7) {
        day++;
        light = (day <= nb_of_days_East) ? 0 : 7;
        isNight = !isNight;
      }
    }

    else {
      isNight = !isNight;
    }
    Serial.print("Day: "); Serial.print(day);
    Serial.print(" Light: "); Serial.println(light);

    for (int i = 1; i < 7; i++) {
    digitalWrite(pins[i], (i == light && !isNight) ? HIGH : LOW);
    }

  }


  if (day > nb_of_days_East + nb_of_days_West) {
    static unsigned long lastTime = 0;
    static int day = 1;
    static int light = 0;
    static bool isNight = false;
  }
}