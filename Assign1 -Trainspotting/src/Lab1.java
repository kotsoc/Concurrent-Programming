import TSim.*;
import java.lang.Thread;
import java.util.concurrent.*;

public class Lab1 {
	Train t1, t2;
	private final Semaphore turnS = new Semaphore(1); // South turn
	private final Semaphore turnM = new Semaphore(1); // Middle Turn
	private final Semaphore CrossN = new Semaphore(1); // North cROSSING
    private final Semaphore CrossS = new Semaphore(1); // South cROSSING

	public Lab1(Integer speed1, Integer speed2) {

		try {
			TSimInterface tsi = TSimInterface.getInstance();
			Train t1 = new Train(1, speed1, tsi);
			Train t2 = new Train(2, speed2, tsi);

			t1.start();
			t2.start();
		} catch (Exception e) {
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

			if (id == 1 ) { // Set the direction of the train true = top to bottom
                dir = true;
                CrossN.acquire(); // Assigning South semaphore initially to N train
                }
			else {
                dir = false;
                 CrossS.acquire(); // Assigning South semaphore initially to S train
                }
			try {
				tsim.setSpeed(id, speed);
				while(true){
					t1Event = tsim.getSensor(id);
					// Get the pos only when we activate the sensor
					if (t1Event.getStatus() == 1) {
						xy = String.valueOf(t1Event.getXpos()) + String.valueOf(t1Event.getYpos());
					}
					else { xy = "0";}
					// Switch on different sensors
					switch (xy) {
						case "153":
						case "155":
							if(dir == false){
								tsim.setSpeed(id, 0);
								Thread.sleep(1000 + (20 * speed));
								tsim.setSpeed(id, -speed);
								dir = true;
								}
								break;
						case "1511":
						case "1513":
							if(dir == true){
							tsim.setSpeed(id, 0);
							Thread.sleep(1000 + (20 * speed));
							tsim.setSpeed(id, -speed);
							dir = false;
							}
							break;
						case "158":
                        if(dir == true) {
                                if (turnM.tryAcquire()) {
                                    tsim.setSwitch(17,7,1); // 1 = Switch left
                                }
                                else{
                                tsim.setSpeed(id, 0);
                                turnM.acquire();
                                tsim.setSpeed(id, speed);
                                tsim.setSwitch(17,7,1);
                                }
                            }
                            else{
                                turnM.release();
                            }
							break;
						case "157":
                            if(dir == true) {
                                if (turnM.tryAcquire()) {
                                    tsim.setSwitch(17,7,2); // 2 = Switch right
                                }
                                else{
                                tsim.setSpeed(id, 0);
                                turnM.acquire();
                                tsim.setSpeed(id, speed);
                                tsim.setSwitch(17,7,2);
                                }
                            }
                            else{
                                turnM.release();
                            }
							break;
							// NEED SEMAPHORE MIDDLE TURN
						case "139":
						case "1310":

							if(dir == false) {
                                if (turnM.tryAcquire()) {
                                tsim.setSwitch(15,9,2); // 2 = Switch right
                                }
                                else{
                                tsim.setSpeed(id, 0);
                                turnM.acquire();
                                tsim.setSpeed(id, speed);
                                tsim.setSwitch(15,9,2);
                                }
                            }
                            else{
                                turnM.release();
                            }
							break;
							// NEED SEMAPHORE MIDDLE TURN
						case "67":
						case "107":
						case "108":
							// nEED SEMAPHORE NORTH CROSSING
						case "197":
							// SEMAPHORE UPPER RIGHT CROSSING  and a switch-- DIRECTION FALSE
                            if(dir == false) { tsim.setSwitch(17,7,1); } // 2 = Switch right
							break;
						case "179":
							// SEMAPHOERE MIDLE RIGHT CROSSING -- DIRECTION TRUE
						case "29":
							// SEMAPHORE MIDLE LEFT CROSSING -- DIRECTION FALSE
						case "111":
							// SEMAPHORE SOUTH CROSSING -- DIRECTION TRUE
						default:
					}
				}
			}

			catch (Exception e) {
				e.printStackTrace(); // or only e.getMessage() for the error
				System.exit(1);
			}
		}
	}
}
