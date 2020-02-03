package panes;

import javafx.scene.control.TextArea;
import javafx.scene.layout.VBox;
import utils.App;

public class OutputPane extends VBox {

    private App app;
    private TextArea textArea;

    public OutputPane(App app) {
        this.app = app;
        this.textArea = new TextArea();
        textArea.setMinHeight(700);
        textArea.setMaxHeight(700);
        textArea.setWrapText(true);

        this.setMinSize(700, 700);
        this.setMaxSize(700, 700);
        this.getChildren().add(textArea);
    }

    public void setText(String text) {
        this.textArea.setText(text);
    }
}
