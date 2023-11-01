
using React::@reactiveStruct;

public struct CCPoint {
    x: float;
    y: float;
}

@reactiveStruct
public extern struct CCNode {
    pos: CCPoint;
    x: float {
        set(value) => this.pos.x = value;
        get => this.pos.x;
    }
    y: float {
        set(value) => this.pos.y = value;
        get => this.pos.y;
    }
}

@append
public extern fun CCNode::addChild(child: CCNode);
