import TSim.*;
import java.lang.Thread;
import java.util.concurrent.*;

public class Lab1 {
 Train t1, t2;
 // South turn
 private final Semaphore turnS = new Semaphore(1);
 // Middle Turn
 private final Semaphore turnM = new Semaphore(1);
 // North cROSSING
 private final Semaphore crossN = new Semaphore(1);
 // South Switch
 private final Semaphore switchS = new Semaphore(1);
 // North Switch
 private final Semaphore switchN = new Semaphore(1);
 // Middle right Switch
 private final Semaphore switchMidR = new Semaphore(1);

 public Lab1(Integer speed1, Integer speed2) {

  try {
   TSimInterface tsi = TSimInterface.getInstance();
   Train t1 = new Train(1, speed1, tsi);
   Train t2 = new Train(2, speed2, tsi);

   t1.start();
   t2.start();
  }
  catch (Exception e) {
   e.printStackTrace(); // or only e.getMessage() for the error
   System.exit(1);
  }
 }
 private class Train extends Thread {
  int id, speed;
  TSimInterface tsim;

  public Train(Integer id, Integer speed, TSimInterface tsi) {
   this.speed = speed;
   this.id = id;
   this.tsim = tsi;
  }

  public void run() {
   TSimInformation trainInf;
   SensorEvent t1Event;
   String xy = new String();
   boolean dir;
   try {
    // Set the direction of the train true = top to bottom
    if (id == 1) {
     dir = true;
     switchN.acquire(); // Assigning South semaphore initially to N train
    }
    else {
     dir = false;
     switchS.acquire(); // Assigning South semaphore initially to S train
    }
    tsim.setSpeed(id, speed);
    // Endless loop to wait for sensor events
    while (true) {
     t1Event = tsim.getSensor(id);
     // Get the pos only when we activate the sensor
     if (t1Event.getStatus() == 1) {
      xy = String.valueOf(t1Event.getXpos()) + String.valueOf(t1Event.getYpos());
     }
     else {
      xy = "0";
     }
     // Switch on different sensors
     switch (xy) {
      case "153":
      case "155":
       if (!dir) {
        tsim.setSpeed(id, 0);
        Thread.sleep(1000 + Math.abs((20 * speed)));
        this.speed = -speed;
        tsim.setSpeed(id, speed);
        dir = true;
       }
       break;
      case "1511":
      case "1513":
       if (dir) {
        tsim.setSpeed(id, 0);
        Thread.sleep(1000 + Math.abs((20 * speed)));
        this.speed = -speed;
        tsim.setSpeed(id, speed);
        dir = false;
       }
       break;
      case "158":
       if (dir) {
        if (turnM.tryAcquire()) {
         tsim.setSwitch(17, 7, 1); // 1 = Switch left
        } else {
         tsim.setSpeed(id, 0);
         turnM.acquire();
         tsim.setSpeed(id, speed);
         tsim.setSwitch(17, 7, 1);
        }
       } else {
        turnM.release();
       }
       break;
      case "157":
       if (dir) {
        if (turnM.tryAcquire()) { // Semaphore for middle turn
         tsim.setSwitch(17, 7, 2); // 2 = Switch right
         switchN.release();
        }
        else {
         tsim.setSpeed(id, 0);
         turnM.acquire();
         tsim.setSpeed(id, speed);
         tsim.setSwitch(17, 7, 2);
         switchN.release();
        }
       }
       else {
        turnM.release();
       }
       break;
      case "139":
       if (!dir) {
        if (turnM.tryAcquire()) {
         tsim.setSwitch(15, 9, 2); // 2 = Switch right
         switchMidR.release();
        }
        else {
         tsim.setSpeed(id, 0);
         turnM.acquire();
         tsim.setSpeed(id, speed);
         tsim.setSwitch(15, 9, 2);
         switchMidR.release();
        }
       }
       else {
        turnM.release();
       }
       break;
      case "1310": // Middle turn semaphore
       if (!dir) {
        if (turnM.tryAcquire()) {
         tsim.setSwitch(15, 9, 1); // 2 = Switch right
        }
        else {
         tsim.setSpeed(id, 0);
         turnM.acquire();
         tsim.setSpeed(id, speed);
         tsim.setSwitch(15, 9, 1);
        }
       }
       else {
        turnM.release();
       }
       break;
      case "67":
      case "85":
       if (dir) {
        if (crossN.tryAcquire()) {}
        else {
         tsim.setSpeed(id, 0);
         crossN.acquire();
         tsim.setSpeed(id, speed);
        }
       } else {
        crossN.release();
       } // Release if coming from the top
       break;
      case "107":
      case "108":
       // Acquire North crossing semaphore
       if (!dir) {
        if (crossN.tryAcquire()) {}
        else {
         tsim.setSpeed(id, 0);
         crossN.acquire();
         tsim.setSpeed(id, speed);
        }
       } else {
        crossN.release();
       } // Release if coming from the top
       break;
      case "197":
       // North switch semaphore
       if (!dir) {
        if (switchN.tryAcquire()) {
         tsim.setSwitch(17, 7, 2); // 2 = Switch right
        } else {
         tsim.setSwitch(17, 7, 1);
        } // 2 = Switch right}

       }
       break;
      case "179":
       // Middle right semaphore - if coming from the top
       if (dir) {
        if (switchMidR.tryAcquire()) {
         tsim.setSwitch(15, 9, 2); // 2 = Switch right
        } else {
         tsim.setSwitch(15, 9, 1); // 1 = Switch left
        }
       }
       break;
      case "29":
       //  mid right semaphore -- direction false(bottom to top)
       if (!dir) {
        if (switchMidR.tryAcquire()) {
         tsim.setSwitch(4, 9, 1);
        } else {
         tsim.setSwitch(4, 9, 2);
        }
       }
       break;
      case "111":
       // semaphore south crossing -- direction true
       if (dir) {
        if (switchS.tryAcquire()) {
         tsim.setSwitch(3, 11, 1); // 1 = Switch left
        } else {
         tsim.setSwitch(3, 11, 2);
        }
       }
       break;
      case "69":
       if (dir) {
        if (turnS.tryAcquire()) {
         tsim.setSwitch(4, 9, 1); // 1 = Switch left
         switchMidR.release(); //Releasing semaphore for middle right switch
        } else {
         tsim.setSpeed(id, 0);
         turnS.acquire();
         tsim.setSwitch(4, 9, 1);
         tsim.setSpeed(id, speed);
         switchMidR.release();
        }
       } else {
        turnS.release(); // Release semaphore for South turn
       }
       break;
      case "610":
       if (dir) {
        if (turnS.tryAcquire()) {
         tsim.setSwitch(4, 9, 2); // 2 = Switch right
        } else {
         tsim.setSpeed(id, 0);
         turnS.acquire();
         tsim.setSwitch(4, 9, 2); // 2 = Switch right
         tsim.setSpeed(id, speed);
        }
       } else {
        turnS.release();
       }
       break;
      case "511":
       if (!dir) {
        if (turnS.tryAcquire()) {
         tsim.setSwitch(3, 11, 1); // 1 = Switch left
         switchS.release(); // Release South switch semaphore
        } else {
         tsim.setSpeed(id, 0);
         turnS.acquire();
         tsim.setSwitch(3, 11, 1);
         tsim.setSpeed(id, speed);
         switchS.release();
        }
       } else {
          turnS.release();
       }
       break;
      case "513":
       if (!dir) {
        if (turnS.tryAcquire()) {
         tsim.setSwitch(3, 11, 2); // 1 = Switch right
        } else {
         tsim.setSpeed(id, 0);
         turnS.acquire();
         tsim.setSwitch(3, 11, 2);
         tsim.setSpeed(id, speed);
        }
       } else {
        turnS.release();
       }
       break;
      default:
     }
    }
   } catch (Exception e) {
    e.printStackTrace(); // or only e.getMessage() for the error
    System.exit(1);
   }
  }
 }
}
