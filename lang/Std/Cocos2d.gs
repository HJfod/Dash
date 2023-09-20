
using React::@reactive;

public struct CCPoint {
    x: float;
    y: float;
}

public extern struct NodeRef {}

@React::reactive
public extern struct CCNode {
    private handle: NodeRef;
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