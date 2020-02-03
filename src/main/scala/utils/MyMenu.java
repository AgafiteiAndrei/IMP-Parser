package utils;

import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;

public class MyMenu extends MenuBar {
    App app;

    public MyMenu(App app) {
        this.app = app;
        init();
    }

    private void init() {
        Menu fileMenu = new Menu("File");

        MenuItem newMenu = new MenuItem("New");
        MenuItem saveMenu = new MenuItem("Save");
        MenuItem loadMenu = new MenuItem("Load");
        MenuItem exitMenu = new MenuItem("Exit");

        newMenu.setOnAction((event) -> app.newFile());
        saveMenu.setOnAction((event) -> app.saveFile());
        loadMenu.setOnAction((event) -> app.loadFile());
        exitMenu.setOnAction((event) -> app.exit());

        fileMenu.getItems().addAll(newMenu, saveMenu, loadMenu, exitMenu);
        this.getMenus().addAll(fileMenu);
    }
}