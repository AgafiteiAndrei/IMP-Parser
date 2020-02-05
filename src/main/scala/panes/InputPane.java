package panes;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.util.Optional;

import javafx.geometry.Pos;
import javafx.scene.control.Button;
import javafx.scene.control.TextArea;
import javafx.scene.control.TextInputDialog;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import utils.App;
import utils.Main.*;

public class InputPane extends VBox {
    private TextArea textArea;
    private HBox buttonsBox;
    private App app;
    private String name = null;

    public InputPane(App app) {
        this.app = app;
        this.setMinSize(500, 700);
        this.setMaxSize(500, 700);

        textArea = new TextArea();
        buttonsBox = new HBox(10);
        textArea.setMaxHeight(620);
        textArea.setMinHeight(620);
        buttonsBox.setMaxHeight(50);
        buttonsBox.setMinHeight(50);

        Button runButton = new Button("Run");
        Button clearButton = new Button("Clear");
        Button astButton = new Button("AST");
        Button clearOutputButton = new Button("Clear output");

        runButton.setOnAction((event) -> run());
        astButton.setOnAction((event) -> runAst());
        clearButton.setOnAction((event) -> clear());
        clearOutputButton.setOnAction((event) -> clearOutput());

        buttonsBox.setAlignment(Pos.CENTER);
        buttonsBox.getChildren().addAll(runButton, astButton, clearButton, clearOutputButton);
        this.getChildren().addAll(textArea, buttonsBox);
    }

    private void run() {
        save();
        String path = "C:\\Programe_facultate\\licenta\\IMP-Parser\\" + this.name +".txt";
        String result = utils.GenerateResult.get_resultEvaluator(path);
        this.app.getPane().getOutputPane().setText(result);
    }

    private void runAst() {
        save();
        String path = "C:\\Programe_facultate\\licenta\\IMP-Parser\\" + this.name+".txt";
        String result = utils.GenerateResult.get_AST(path);
        this.app.getPane().getOutputPane().setText(result);
    }

    private void clear() {
        textArea.setText("");
    }

    private void clearOutput() {
        this.app.getPane().getOutputPane().setText("");
    }

    public void createNew() {
        clear();
        clearOutput();
        setName(null);
    }

    public void save() {
        File file = null;
        if (name == null) {
            TextInputDialog dialog = new TextInputDialog("");
            dialog.setHeaderText("Enter the program name:");
            dialog.setContentText("Name:");

            Optional<String> dialogResult = dialog.showAndWait();
            try {
                name = dialogResult.get();
            } catch (Exception e) {
                return;
            }

            this.setName(name);
        }

        file = new File("C:\\Programe_facultate\\licenta\\IMP-Parser\\" + name + ".txt");

        try (FileOutputStream fileOutputStream = new FileOutputStream(file);
             BufferedOutputStream bufferedOutputStream = new BufferedOutputStream(fileOutputStream)) {
            String text = getText();
            bufferedOutputStream.write(text.getBytes());
            bufferedOutputStream.flush();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public void load(String path) {
        StringBuffer stringBuffer = new StringBuffer();
        try (FileInputStream fileInputStream = new FileInputStream(path);
             BufferedInputStream bufferedInputStream = new BufferedInputStream(fileInputStream)) {
            while (bufferedInputStream.available() > 0) {
                stringBuffer.append((char) bufferedInputStream.read());
            }
        } catch (Exception e) {
            e.printStackTrace();
        }

        String name = new File(path).getName();
        this.setName(name.substring(0, name.length() - 4));
        this.setText(stringBuffer.toString());
    }

    public String getText() {
        return textArea.getText();
    }

    public void setText(String text) {
        this.textArea.setText(text);
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}