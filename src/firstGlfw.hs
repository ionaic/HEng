import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.IORef
import Control.Monad

rendLoop = do 
        -- set up a flag to check if the window is closed
        quit <- newIORef False
 
        -- set up the window close callback function (i'm a little shaky here)
        -- i believe it's just a flag and it lets you stop the program if 
        -- quit is true, because type of the RHS of the equation is an
        -- IO Bool, which is really just a Bool in an IO monad....
        --
        -- UPDATE: the windowCloseCallback I belive just needs to be a flag
        --   of close or no.  return is a function which puts something into
        --   a monad, and >> just sticks two monads together and gives you a 
        --   monad containing the second arg, so as far as i can tell, it ends
        --   up being a monad (IO monad specifically) containing the IORef and
        --   the returned value, which we could read as such:
        --
        --          writeIoRef quit True >>= return readIORef quit
        --
        --   but that's rather pointless, since we just set quit to True, and
        --   therefore know its value already.  the existing code therefore
        --   writes to the IORef true and returns, what i'm guessing is a success
        --   status.
        --
        --   alternatively could use the following as callback:
        --          do
        --              GLFW.closeWindow
        --              GLFW.terminate
        --              return True
        GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

        -- start main loop and define it using where clause
        loop quit 
        where
            loop quit = do
                -- Tells GLFW to look for event callbacks
                GLFW.waitEvents
                
                -- read if quit has been called or not
                q <- readIORef quit
                
                -- unless quit is true, continue loop.  otherwise we're done!
                unless q $ loop quit

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
