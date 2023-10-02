
using React::@reactiveStruct;

public struct CCPoint {
    x: float;
    y: float;
}

@reactiveStruct
public extern struct CCNode {
    pos: CCPoint;
    x: float {
        set(value) => pos.x = value;
        get => pos.x;
    }
    y: float {
        set(value) => pos.y = value;
        get => pos.y;
    }
}

@append
public extern fun CCNode::addChild(child: CCNode);
