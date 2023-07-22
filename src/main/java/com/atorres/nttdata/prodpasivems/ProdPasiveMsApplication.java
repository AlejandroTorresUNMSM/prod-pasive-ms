package com.atorres.nttdata.prodpasivems;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.cloud.client.discovery.EnableDiscoveryClient;
import reactivefeign.spring.config.EnableReactiveFeignClients;

@EnableDiscoveryClient
@EnableReactiveFeignClients
@SpringBootApplication
public class ProdPasiveMsApplication {

	public static void main(String[] args) {
		SpringApplication.run(ProdPasiveMsApplication.class, args);
	}

}
