# 

**Build Docker Image**

1. Download the Dockerfile
2. Run the commands below to move to your directory where you download Dockerfile
    
    ```python
    # code
    cd [directory]
    # example
    cd C:/git 
    ```
    
3. Run the commands below to build Dockerfile
(You can get your GIT_ACCESS_TOKEN in [github.com](http://github.com/). Follow instructions below.)
https://docs.github.com/en/authentication/keeping-your-account-and-data-secure/managing-your-personal-access-tokens
    
    ```python
    # code
    docker build --build-arg GIT_ACCESS_TOKEN=[insert-access-token-here] -t [image_name]:[image_tag] .
    # example
    docker build --build-arg GIT_ACCESS_TOKEN=ghd123123 -t rpack-atlas:v2.12.0 .
    ```
    
4. Using the commands below, you can see the docker image the you build using Dockerfile
    
    ```python
    docker images
    ```
    

**Run Docker Container**

1. Use the following command to run docker container
    
    ```powershell
    # code
    docker run --name [conatainer_name] -e USER=user -e PASSWORD=password -p 8787:8787 [image_name]:[image_tag]
    # example
    docker run --name rpack-atlas -e USER=user -e PASSWORD=password -p 8787:8787 rpack-atlas:v2.12.0
    ```
    
2. USER and PASSWORD that you provided will be used for signing in to Rstudio later
