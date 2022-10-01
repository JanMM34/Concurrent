import TSim.*;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.Semaphore;

public class Lab1 {
    public Lab1(int speed1, int speed2) {

        Map.init();

        Rail Rail1 = (Rail) Map.getRail(new Position(15, 7));
        Rail Rail2 = (Rail) Map.getRail(new Position(5, 11));

        Train train1 = new Train(1, speed1, Direction.UP, Rail1);
        Train train2 = new Train(2, speed2, Direction.DOWN, Rail2);

        train1.start();
        train2.start();
    }
}

class Train extends Thread {
    private int id;
    private int speed;
    private Rail prevRail;
    private Direction travelDirection;
    private TSimInterface tsi = TSimInterface.getInstance();

    public Train(int id, int speed, Direction travelDirection, Rail initialRail) {
        this.id = id;
        this.speed = speed;
        this.prevRail = null;
        this.travelDirection = travelDirection;
        initialRail.acquire();
    }

    @Override
    public void run() {
        try {
            tsi.setSpeed(id, speed);
            while (true)
                getSensorEvent(tsi.getSensor(this.id));
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    private void getSensorEvent(SensorEvent event) throws Exception {
        Position sensor = new Position(event.getXpos(), event.getYpos());
        Rail currRail = Map.getRail(sensor);

        
        if (currRail instanceof Section) {

            Section currSection = (Section) currRail;
            
            //start of rail
            if (event.getStatus() == SensorEvent.INACTIVE && (this.travelDirection != currRail.getTravelDirection())) {
                if (prevRail != null)
                    prevRail.release();
                prevRail = null;
            }
            //end of rail
            if (event.getStatus() == SensorEvent.ACTIVE && (this.travelDirection == currRail.getTravelDirection())) {
                tsi.setSpeed(id, 0);
                if (currSection.acquireNextRail()) {
                    prevRail = currSection;
                } else {
                    Thread.sleep(1000 + (20 * Math.abs(speed)));

                    if (this.travelDirection == Direction.UP) {
                        this.travelDirection = Direction.DOWN;
                    } else {
                        this.travelDirection = Direction.UP;
                    }
                    this.speed = -this.speed;
                }
                tsi.setSpeed(id, speed);
            }
        } else {
            //end of rail
            if (event.getStatus() == SensorEvent.INACTIVE && (this.travelDirection == currRail.getTravelDirection())) {
                currRail.release();

            }
            //start of rail
            if (event.getStatus() == SensorEvent.ACTIVE && (this.travelDirection != currRail.getTravelDirection())) {
                tsi.setSpeed(id, 0);
                currRail.acquire();
                tsi.setSpeed(id, speed);
            }
        }
    }

}



class Map {


    //static map to get the rail associated to a sensor
    public static HashMap<Position, Rail> sensorsRail = null;

    public static void init() {

        sensorsRail = new HashMap<>();
        
        ArrayList<Semaphore> semaphores = new ArrayList<>();
        
        Position[] switches = new Position[4];
        Rail[] rails = new Rail[4];
        Section[] sections = new Section[16];
    
    

        for (int i = 0; i < 9; i++) {
            semaphores.add(new Semaphore(1));
            
        }


        switches[0] = new Position(3, 11);
        switches[1] = new Position(4, 9);
        switches[2] = new Position(15, 9);
        switches[3] = new Position(17, 7);

        rails[0] = new Rail(Direction.DOWN, semaphores.get(0), new Position(6, 7));
        rails[1] = new Rail(Direction.DOWN, semaphores.get(0), new Position(8, 5));
        rails[2] = new Rail(Direction.UP, semaphores.get(0), new Position(10, 7));
        rails[3] = new Rail(Direction.UP, semaphores.get(0), new Position(9, 8));

        sections[0] = new Section(Direction.DOWN, semaphores.get(1), new Position(5, 11));
        sections[1] = new Section(Direction.DOWN, semaphores.get(2), new Position(3, 13));
        sections[2] = new Section(Direction.DOWN, semaphores.get(3), new Position(2, 9));
        sections[3] = new Section(Direction.DOWN, semaphores.get(4), new Position(13, 9));
        sections[4] = new Section(Direction.DOWN, semaphores.get(5), new Position(13, 10));
        sections[5] = new Section(Direction.DOWN, semaphores.get(6), new Position(19, 7));
        sections[6] = new Section(Direction.DOWN, semaphores.get(7), new Position(15, 3));
        sections[7] = new Section(Direction.DOWN, semaphores.get(8), new Position(15, 5));

        sections[8] = new Section(Direction.UP, semaphores.get(1), new Position(15, 11));
        sections[9] = new Section(Direction.UP, semaphores.get(2), new Position(15, 13));
        sections[10] = new Section(Direction.UP, semaphores.get(3), new Position(1, 11));
        sections[11] = new Section(Direction.UP, semaphores.get(4), new Position(6, 9));
        sections[12] = new Section(Direction.UP, semaphores.get(5), new Position(6, 10));
        sections[13] = new Section(Direction.UP, semaphores.get(6), new Position(17, 9));
        sections[14] = new Section(Direction.UP, semaphores.get(7), new Position(15, 7));
        sections[15] = new Section(Direction.UP, semaphores.get(8), new Position(15, 8));

        sections[0].addcrossing(sections[2], switches[0], TSimInterface.SWITCH_LEFT);
        sections[1].addcrossing(sections[2], switches[0], TSimInterface.SWITCH_RIGHT);
        sections[2].addcrossing(sections[3], switches[1], TSimInterface.SWITCH_LEFT);
        sections[2].addcrossing(sections[4], switches[1], TSimInterface.SWITCH_RIGHT);
        sections[3].addcrossing(sections[5], switches[2], TSimInterface.SWITCH_RIGHT);
        sections[4].addcrossing(sections[5], switches[2], TSimInterface.SWITCH_LEFT);
        sections[5].addcrossing(sections[6], switches[3], TSimInterface.SWITCH_RIGHT);
        sections[5].addcrossing(sections[7], switches[3], TSimInterface.SWITCH_LEFT);

        sections[10].addcrossing(sections[8], switches[0], TSimInterface.SWITCH_LEFT);
        sections[10].addcrossing(sections[9], switches[0], TSimInterface.SWITCH_RIGHT);
        sections[11].addcrossing(sections[10], switches[1], TSimInterface.SWITCH_LEFT);
        sections[12].addcrossing(sections[10], switches[1], TSimInterface.SWITCH_RIGHT);
        sections[13].addcrossing(sections[11], switches[2], TSimInterface.SWITCH_RIGHT);
        sections[13].addcrossing(sections[12], switches[2], TSimInterface.SWITCH_LEFT);
        sections[14].addcrossing(sections[13], switches[3], TSimInterface.SWITCH_RIGHT);
        sections[15].addcrossing(sections[13], switches[3], TSimInterface.SWITCH_LEFT);
    }

    public static Rail getRail(Position sensor) {
        if (sensorsRail == null) {
            throw new RuntimeException();
        }
        return sensorsRail.get(sensor);
    }

}

enum Direction {
    UP,
    DOWN,
}

class Position {
    public final int x;
    public final int y;

    public Position(int x, int y) {
        this.x = x;
        this.y = y;
    }

    @Override
    public int hashCode() {
        return x * 100 + y;
    }

    @Override
    public boolean equals(Object pos) {
        return this.hashCode() == pos.hashCode();
    }
}



class Rail {
    private Semaphore semaphore;
    private Direction travelDirection;

    public Rail(Direction travelDirection, Semaphore semaphore, Position sensor) {
        this.semaphore = semaphore;
        this.travelDirection = travelDirection;
        Map.sensorsRail.put(sensor, this);
    }

    public Direction getTravelDirection() {
        return this.travelDirection;
    }


    //acquire if we can, doesn't block
    public boolean tryAcquire() {
        return semaphore.tryAcquire();
    }


    //if we can't acquire it blocks
    public void acquire() {
        try {
            semaphore.acquire();
        } catch (InterruptedException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }

    public void release() {
        semaphore.release();
    }


}

class Section extends Rail{

    private ArrayList<Crossing> crossings;

    public Section(Direction travelDirection, Semaphore semaphore, Position sensor) {
        super(travelDirection, semaphore, sensor);
        crossings = new ArrayList<>();
        
    }

    //we TryToAcquire every rail of the crossing and we only block the thread if it's the last one
    //if there is no nextRail return false
    public boolean acquireNextRail() throws CommandException {
        for (int i = 0; i < crossings.size(); i++) {
            Crossing crossing = crossings.get(i);
            Rail nextRail = crossing.getNextRail();

            if (i == crossings.size() - 1) {
                nextRail.acquire();
                crossing.makesectionswitch();
                return true;
            } else if (nextRail.tryAcquire()) {
                crossing.makesectionswitch();
                return true;
            }
        }
        return false;
    }
    
    public void addcrossing(Rail nextRail, Position switchPos, int switchDir) {
        crossings.add(new Crossing(nextRail, switchPos, switchDir));
    
     
    }

    public ArrayList<Crossing> getCrossings() {
        return crossings;
    }
}

class Crossing {
    private Rail nextRail;
    private Position switchPos;
    private int switchDir;

    public Crossing(Rail nextRail, Position switchPos, int switchDir) {
        this.nextRail = nextRail;
        this.switchPos = switchPos;
        this.switchDir = switchDir;
    }

    public Rail getNextRail() {
        return this.nextRail;
    }

    public void makesectionswitch() throws CommandException {
        TSimInterface.getInstance().setSwitch(switchPos.x, switchPos.y, switchDir);
    }
}
