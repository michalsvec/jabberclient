--
-- Project: jabclient
-- Author:  jirka
--
module Global where

-- GLOBALNI PROMENNE ---------------------------------------

-- pro EnvInt typ
import System.Environment
import Data.IORef
import Control.Monad.State

import TCPConnection

type EnvTCPConnection = IORef [(String, IORef TCPConnection)]

-- empty environment
nullEnvTCPConnection :: IO EnvTCPConnection
nullEnvTCPConnection = newIORef []

setVarTCPConnection :: EnvTCPConnection -> String -> TCPConnection -> IO TCPConnection
setVarTCPConnection envRef var value = do 
    -- read the ref to the environment
    env <- liftIO $ readIORef envRef
    -- lookup the value in the environment
    case (lookup var env) of
        -- we have a reference
        (Just ref) -> do
                        writeIORef ref value
                        return value
        -- this variable doesn't exist in the environment
        Nothing -> do
                    -- create a new reference
                    valueRef <- newIORef value
                    -- expand the environent and write to the ref
                    writeIORef envRef ((var, valueRef) : env)
                    return value

getVarTCPConnection :: EnvTCPConnection -> String -> IO TCPConnection
getVarTCPConnection envRef var = do
    env <- liftIO $ readIORef envRef
    case (lookup var env) of
        (Just ref) -> do 
                        val <- readIORef ref
                        return val

type EnvInt = IORef [(String, IORef Int)]

-- empty environment
nullEnvInt :: IO EnvInt
nullEnvInt = newIORef []

setVarInt :: EnvInt -> String -> Int -> IO Int
setVarInt envRef var value = do 
    -- read the ref to the environment
    env <- liftIO $ readIORef envRef
    -- lookup the value in the environment
    case (lookup var env) of
        -- we have a reference
        (Just ref) -> do
                        writeIORef ref value
                        return value
        -- this variable doesn't exist in the environment
        Nothing -> do
                    -- create a new reference
                    valueRef <- newIORef value
                    -- expand the environent and write to the ref
                    writeIORef envRef ((var, valueRef) : env)
                    return value

getVarInt :: EnvInt -> String -> IO Int
getVarInt envRef var = do
    env <- liftIO $ readIORef envRef
    case (lookup var env) of
        (Just ref) -> do 
                        val <- readIORef ref
                        return val

-- GLOBALNI PROMENNE ---------------------------------------
