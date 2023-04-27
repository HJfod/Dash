
decl MyNode {
    let count: int = 0;
    CCNode {
        layout: ColumnLayout {};
        CCLabelBMFont {
            text: "Clicked {count} times";
        }
        CCMenu {
            CCMenuItemSpriteExtra {
                ButtonSprite {
                    text: "Click me!";
                }
                clicked: () => {
                    count += 1;
                }
            }
        }
    }
}

MyNode {
    count: 5;
}
