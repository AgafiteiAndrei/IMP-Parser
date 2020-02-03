package utils;

import javafx.scene.control.SplitPane;
import javafx.scene.layout.VBox;
import panes.InputPane;
import panes.OutputPane;

public class PrincipalPane extends VBox {
    private SplitPane splitPane;
    private InputPane inputPane;
    private OutputPane outputPane;
    private App app;

    public PrincipalPane(App app){
        this.app = app;
        inputPane = new InputPane(app);
        outputPane = new OutputPane(app);

        splitPane = new SplitPane(inputPane, outputPane);
        this.getChildren().add(splitPane);
    }

    public InputPane getInputPane() {
        return inputPane;
    }

    public void setInputPane(InputPane inputPane) {
        this.inputPane = inputPane;
    }

    public OutputPane getOutputPane() {
        return outputPane;
    }

    public void setOutputPane(OutputPane outputPane) {
        this.outputPane = outputPane;
    }
}
