atom.commands.add 'atom-text-editor',

    # Move 10 lines below
    'custom:move-10-lines-down': ->
        textEditor = atom.workspace.getActiveTextEditor()
        for cursor in textEditor.getCursors()
            position = cursor.getScreenPosition()
            cursor.setScreenPosition([position.row+10, position.column], autoscroll: true)

    # Move 10 lines up
    'custom:move-10-lines-up': ->
        textEditor = atom.workspace.getActiveTextEditor()
        for cursor in textEditor.getCursors()
            position = cursor.getScreenPosition()
            cursor.setScreenPosition([position.row-10, position.column], autoscroll: true)
