
extern fun onButtonClicked() -> void;

public fun buildExternUI() -> CCNode {
    root: CCNode {
        layout: ColumnLayout {}
        CCLabelBMFont {
            text: "Hi mom!",
            id: "my-label",
        }
        CCMenu {
            CCMenuItemSpriteExtra {
                let clicks: int = 0;
                sprite: CCSprite {
                    frame: "GJ_button_01.png",
                }
                clicked: () => {
                    clicks += 1;
                    root.getChildByID("my-label").text = "Clicked {clicks} times";
                    onButtonClicked();
                }
            }
        }
    }
}
