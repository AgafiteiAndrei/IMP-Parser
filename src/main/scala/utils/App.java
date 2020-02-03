package utils;

import java.io.File;

import javafx.application.Application;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.stage.Stage;

public class App extends Application {
    private Stage primaryStage;
    private PrincipalPane pane;

    @Override
    public void start(Stage primaryStage) {
        this.primaryStage = primaryStage;
        pane = new PrincipalPane(this);

        MyMenu menu = new MyMenu(this);

        VBox layout = new VBox(10);
        layout.getChildren().addAll(menu, pane);
        final Scene scene = new Scene(layout, 1200, 700);
        primaryStage.setResizable(false);

        primaryStage.setScene(scene);
        primaryStage.setTitle("IMP Parser");
        primaryStage.show();
    }

    public void newFile() {
        this.pane.getInputPane().createNew();
    }

    public void saveFile() {
        this.pane.getInputPane().save();
    }

    public void loadFile() {
        FileChooser fileChooser = new FileChooser();
        fileChooser.setInitialDirectory(new File("C:\\Programe_facultate\\sbt worksheet\\IMP-Parser\\"));
        File file = fileChooser.showOpenDialog(null);

        this.pane.getInputPane().load(file.getAbsolutePath());
    }

    public void exit() {
        this.primaryStage.close();
    }


    public PrincipalPane getPane() {
        return pane;
    }

    public void setPane(PrincipalPane pane) {
        this.pane = pane;
    }
}