import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL as GL
import Data.IORef --for the IORef state stuff
import Control.Monad --for stuff like  liftM and unless
import Data.Array.MArray --for the array operations (like newListArray)
import Data.Array.Storable --for some of the Ptr stuff
import Foreign.Ptr --for more of the pointer stuff
import Foreign.Storable (sizeOf)--for sizeOf

data Resources = Resources {vertexArrayVBO :: BufferObject,
                            elementBufferVBO :: BufferObject}

data Shaders = Shaders {vertexShader :: [VertexShader],
                        fragmentShader :: [FragmentShader],
                        program :: Program,
                        inPosition :: AttribLocation,
                        inColor :: AttribLocation}

--data AttrLoc = AttrLoc {inPosition :: AttribLocation,
--                        inColor :: AttribLocation}

-- function which simply returns the GLfloat array of raw vertex data
rawVertexData :: [GLfloat]
rawVertexData = [-1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 0.0]

-- function which simply returns the GLfloat array of raw color data
rawColorData :: [GLfloat]
rawColorData = [1.0, 0.0, 0.0,  0.0, 1.0, 0.0,  0.0, 0.0, 1.0]

-- genObjectNames is _____
-- bindBuffer is a function bindBuffer :: BufferTarget -> ionno something which
--      basically lets you set that type of buffer to something?
-- newListArray makes an array from a list, taking an argument (i, j) where i is
--      the lower bound of indices and j is the upper bound (this array goes
--      from 0 to length)
--
-- a really cool, elegant solution to finding size of the pointer, found on
--      interwebs (http://fhtr.blogspot.com/2009/06/haskell-opengl-utilities.html)
--where ptrsize [] = toEnum 0
--          ptrsize x:xs = toEnum $ length elems * (sizeOf x)
initBuffer :: [GLfloat] -> IO BufferObject
initBuffer rawData = genObjectNames 1 >>= 
        \x@(vbo:t) -> bindBuffer ArrayBuffer $= Just vbo >> 
            newListArray (0, length rawData - 1) rawData >>=
            \arr -> withStorableArray arr (\ptr ->
                bufferData ArrayBuffer $= (toEnum $ length rawData * (sizeOf $ head rawData), ptr, StaticDraw)) >> 
            bindBuffer ArrayBuffer $= Nothing >>
            return vbo

-- builds Resources by calling initBuffer on the vertex and color data
initGeometry :: IO Resources
initGeometry = liftM2 Resources (initBuffer rawVertexData) (initBuffer rawColorData)

-- function to initialize shaders
--initShaders :: (IO Shaders, StateVar AttrLoc)
initShaders :: IO Shaders
initShaders = loadShader "minimal.vert" >>= \vs ->
                loadShader "minimal.frag" >>= \fs ->
                    compileShader vs >>
                    compileShader fs >>
                        --linkShaderProgram vs fs >>= \prgm -> 
                        linkShaderProgram vs fs >>= \(prgm, inPos, inCol) ->
                            return $ Shaders vs fs (fst prgm) (snd prgm) (thrd prgm)
                                --return Shaders vs fs (fst prgm) (snd prgm) (thrd prgm)
                where
                    loadShader path = readFile path >>= \srcFile ->
                        genObjectNames 1 >>= \[shader] ->
                            shaderSource shader $= [srcFile] >>
                                return shader

                    --linkShaderProgram vs@(vert:vertTail) fs@(frag:fragTail) = genObjectNames 1 >>= \names@(program:namesTail) -> 
                    linkShaderProgram vs fs = genObjectNames 1 >>= \names@(program:namesTail) -> 
                        attachedShaders program $= (vs, fs) >>
                        --liftM2 $ AttrLoc attribLocation program "in_Position" attribLocation program "in_Color" >>= \attr -> 
                        get (attribLocation program "in_Position") >>= \inpos ->
                        get (attribLocation program "in_Color") >>= \incol->
                        linkProgram program >> 
                        return (program, inpos, incol)
                        --Shaders `liftM` vs `ap` fs `ap` program `ap` inpos `ap` incol
                        --liftM5 Shaders vert frag program inpos incol
                        --linkProgram program >> return (Shaders vs fs program, attr)
                    thrd (a, b, c) = c
                        
-- main rendering function
render :: Resources -> IO Bool
render res = GL.clear [GL.ColorBuffer] >>
        bindBuffer ArrayBuffer $= Just arr >>
        drawArrays Triangles (fst arrBounds) (read . show . snd $ arrBounds :: NumArrayIndices) >>
        return True
        where
            arr = vertexArrayVBO res
            arrBounds = (0, length rawVertexData - 1)

-- main rendering function to set some things up and start main render loop
rendMain :: IO Bool
rendMain =
        -- set a flag to check if the window is closed
        newIORef False >>= \quit -> 

        -- set up the window close callback function
        GLFW.windowCloseCallback $= (writeIORef quit True >> 
                                GLFW.closeWindow >> 
                                GLFW.terminate >> 
                                return True) >>

        -- Set the clear color
        GL.clearColor $= Color4 0 0 0 0 >>

        -- Set clearDepth
        GL.clearDepth $= 1.0 >>

        -- enable and set depth function
        -- replaces glEnable(GL_DEPTH_TEST) and glDepthFunc(GL_LEQUAL)
        depthFunc $= Just Lequal >> 
        
        -- initialize geometry
        initGeometry >>= \res ->
        
        -- initialize shaders
        initShaders >>= \shaders ->

        -- start main loop and define it using where clause
        loop res quit
        where
            loop res quit =
                -- Tells GLFW to look for event callbacks
                GLFW.waitEvents >>
                
                -- Actively redraw window
                render res >> GLFW.swapBuffers >>

                -- read if quit has been called or not
                -- if quit is false, continue loop.  otherwise we're done!
                readIORef quit >>= \q -> case q of 
                    False -> loop res quit
                    _ -> return True

main =
    -- initialize GLFW
    GLFW.initialize >>

    -- open a window 
    GLFW.openWindow (GL.Size 300 300) [GLFW.DisplayAlphaBits 8] GLFW.Window >>

    -- set window title
    GLFW.windowTitle $= "butts" >>
    rendMain >>

    -- in case the loop ends for some other reason
    GLFW.closeWindow >>
    GLFW.terminate
