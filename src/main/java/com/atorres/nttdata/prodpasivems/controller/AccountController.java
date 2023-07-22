package com.atorres.nttdata.prodpasivems.controller;

import com.atorres.nttdata.prodpasivems.client.ClientApiClient;
import com.atorres.nttdata.prodpasivems.model.clientms.ClientDto;
import com.atorres.nttdata.prodpasivems.model.dto.AccountDto;
import com.atorres.nttdata.prodpasivems.service.AccountService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@RestController
@RequestMapping("/api/account")
@Slf4j
public class AccountController {
	@Autowired
	AccountService accountService;
	@Autowired
	ClientApiClient clientApiClient;

	/**
	 * Metodo para traer la cuenta
	 * @param productId id producto
	 * @return cuenta
	 */
	@GetMapping(value = "/{productId}",produces = MediaType.TEXT_EVENT_STREAM_VALUE)
	public Mono<AccountDto> getAccount(
					@PathVariable String productId){
		return accountService.getAccount(productId)
						.doOnNext(account -> log.info("Cuenta encontrada con exito"));
	}

	@GetMapping(value = "/clientes",produces = MediaType.TEXT_EVENT_STREAM_VALUE)
	public Flux<ClientDto> getClient(){
		return clientApiClient.getClients()
						.doOnNext(cl -> log.info("Clientes encontrado: "+cl.getId()));
	}

	@GetMapping(value = "/clientes/{id}",produces = MediaType.TEXT_EVENT_STREAM_VALUE)
	public Mono<ClientDto> getClients(
					@PathVariable String id){
		return clientApiClient.getClientById(id)
						.doOnNext(cl -> log.info("Clientes encontrado: "+cl.getId()));
	}
}
