// include the lcd library
#include <LiquidCrystal.h>

// initialize the library with the numbers of the arduino pins 
LiquidCrystal lcd(12, 11, 5, 4, 3, 2);

#include <IRremote.h>
const int RECV_PIN = 7;
IRrecv irrecv(RECV_PIN);
decode_results results;

signed short minutes, secondes, millisecondes;
char timeline[16];

int homeScore;
int visitorScore;

void setup(){
  irrecv.enableIRIn();
  irrecv.blink13(true);
  lcd.begin(16, 2); 
  
  setupScreen(); 
}
void setupScreen() 
{ 
  lcd.clear(); 
  // Make the home score on the left, and the visitor several spaces away
  lcd.setCursor(3,0); 
  lcd.print("Home  Visitor"); 
  // start it at zeros 
  homeScore = 0; 
  visitorScore = 0; 

  


}

void loop() { 



  
  
  if (irrecv.decode(&results)){

        switch(results.value){
          case 0xFFC23D:
          lcd.setCursor(0, 1);
          sprintf(timeline, "%0.2d",  secondes);
          lcd.print(timeline);
  
          delay(1000);
          secondes++;
  
          if (secondes == 60)
          {
          lcd.setCursor(0,1);
           lcd.print("X");
          }
        }
        
        
        switch(results.value){
          case 0xFFA25D: //Keypad button "CH-"
          homeScore++; 
          delay(500); 
        }
        switch(results.value){
          case 0xFFE01F: //Keypad button "-"
          homeScore++; 
          homeScore++;
          homeScore++;
          delay(500); 
        }

        switch(results.value){
          case 0xFFE21D: //Keypad button "Ch+"
          visitorScore++; 
          delay(500);
        }
        switch(results.value){
          case 0xFFA857: //Keypad button "Ch+"
          visitorScore++;
          visitorScore++;
          visitorScore++; 
          delay(500);
        }

        switch(results.value){
          case 0xFF629D: //Keypad button "CH"
          setupScreen();
        }
        
        switch(results.value){
          case 0xFF22DD: //Keypad button "CH"
          homeScore--;
          delay(500);
        }
        switch(results.value){
          case 0xFF02FD: //Keypad button "CH"
          visitorScore--;
          delay(500);
        }

        irrecv.resume();
          }

        
  lcd.setCursor(6, 1);
  lcd.print(homeScore); 
  lcd.setCursor(15, 1); 
  lcd.print(visitorScore);  
  
}
