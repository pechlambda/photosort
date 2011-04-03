module Manager (
      processDirectory
    ) where
    
import System.Directory
import System.FilePath
import Photo

-- возвращает директорию с фотографиями, считывая ее путь из файла настроек
picturesDir :: IO FilePath
picturesDir = 
    getAppUserDataDirectory "photosort" >>= createAndSet >> (readDefault "pictures" "Pictures") >>= return
    where
        --создаем директорию, если нужно, и устанавливаем её текущей
        createAndSet appDir = createDirectoryIfMissing True appDir >> (setCurrentDirectory appDir)
        --проверяем, существует ли данный файл
        readDefault fileName def = doesFileExist fileName >>= (readSafe fileName def)
        --если не существует, создаем его и пишем туда дефолтное значение
        readSafe fileName def False = do
            home <- getHomeDirectory
            let fullPath = home </> def
            writeFile fileName fullPath
            return fullPath
        --иначе просто читаем значение
        readSafe fileName _ True = readFile fileName >>= return

--рекурсивная обработка всех каталогов
processDirectory :: FilePath -> IO ()
processDirectory dir = 
    getDirectoryContents >>= checkItems >>= (mapM_ processSingle)
    where
        --по заданному списку содержимого каталога возвращаем кортежи с маркером "каталог" для каждого элемента
        checkItems xs = mapM singleCheck xs
            where
                singleCheck path = (doesDirectoryExist path) >>= (\isDirectory -> return (path, isDirectory))
        --обработка элемента: если каталог - входим в рекурсию, иначе обрабатываем фотографию
        processSingle (path, True) = processDirecotry path
        processSingle (path, False) = do
            pictures <- picturesDir
            --ловим экзепшн, ведь возможно этот файл вовсе не фотография
            maybeDate <- catch (getTime path) nothing
            copyPhoto pictures maybeDate
                where
                    nothing _ = Nothing
                    -- безопасное копирование
                    copyPhoto pictures Nothing = return ()
                    copyPhoto pictures (Just date) = do
                        let newPath = pictures </> (show date)
                        createDirectoryIfMissing True newPath
                        copyFile path newPath
