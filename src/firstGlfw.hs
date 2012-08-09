import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.IORef
import Control.Monad
import System.Environment

rendLoop = do 
        -- set up a flag to check if the window is closed
        quit <- newIORef False
 
        -- set up the window close callback function (i'm a little shaky here)
        -- i believe it's just a flag and it lets you stop the program if 
        -- quit is true, because type of the RHS of the equation is an
        -- IO Bool, which is really just a Bool in an IO monad....
        GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

        -- start main loop and define it using where clause
        -- waitForPress
        loop quit 
        where
            loop quit = do
                GLFW.waitEvents
                q <- readIORef quit
                unless q $ loop quit
           --  
           --  waitForPress = do
           --      GLFW.keyCallback $= \b k ->
           --          when (b == GLFW.KeyEsc && k == GLFW.Press)

main = do
    -- initialize GLFW
    GLFW.initialize

    -- open a window 
    GLFW.openWindow (GL.Size 300 300) [GLFW.DisplayAlphaBits 8] GLFW.Window
    -- set window title
    GLFW.windowTitle $= "butts"
    GL.clearColor $= Color4 0 0 0 0
    rendLoop
    GLFW.closeWindow
    GLFW.terminate
